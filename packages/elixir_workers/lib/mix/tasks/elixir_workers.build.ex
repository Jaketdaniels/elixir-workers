defmodule Mix.Tasks.ElixirWorkers.Build do
  use Mix.Task

  @shortdoc "Compile Elixir code and pack into .avm for Cloudflare Workers"

  @moduledoc """
  Compiles your Elixir project and packages it into an .avm archive
  that runs on AtomVM inside Cloudflare Workers.

      $ mix elixir_workers.build

  This task:
  1. Compiles your Elixir code with `mix compile`
  2. Collects stdlib, framework, and user .beam files
  3. Packs everything into `_build/worker/app.avm`
  4. Sets up the `_build/worker/` directory with JS runtime and WASM binary

  Build artifacts go in `_build/worker/`. User config (`wrangler.jsonc`,
  `package.json`) lives at the project root.
  """

  @impl Mix.Task
  def run(_args) do
    started = System.monotonic_time(:millisecond)

    # 1. Compile user code
    step("compiling", "Elixir code")
    Mix.Task.run("compile", ["--no-deps-check"])

    app = Mix.Project.config()[:app]
    build_path = Mix.Project.build_path()
    project_root = File.cwd!()
    worker_dir = Path.join(project_root, "_build/worker")

    # Resolve the elixir_workers dependency path
    deps_path = Mix.Project.deps_path()
    ew_dep_path = Path.join(deps_path, "elixir_workers")

    # If running inside the elixir_workers package itself (development), use priv/ directly
    ew_priv =
      if File.dir?(Path.join(ew_dep_path, "priv")) do
        Path.join(ew_dep_path, "priv")
      else
        # Running from the package itself
        Application.app_dir(:elixir_workers, "priv")
      end

    # 2. Collect user .beam files
    ebin_dir = Path.join([build_path, "lib", to_string(app), "ebin"])

    unless File.dir?(ebin_dir) do
      Mix.raise("No compiled .beam files found at #{ebin_dir}. Run `mix compile` first.")
    end

    # 3. Merge user beams + stdlib beams into a temp directory
    tmp_beams = Path.join([build_path, "elixir_workers_beams"])
    File.rm_rf!(tmp_beams)
    File.mkdir_p!(tmp_beams)

    # Collect all available beams by category
    stdlib_dir = Path.join(ew_priv, "stdlib")
    ew_ebin = Path.join([build_path, "lib", "elixir_workers", "ebin"])

    stdlib_beams = list_beams(stdlib_dir)
    fw_beams = list_beams(ew_ebin)

    user_beams =
      ebin_dir
      |> File.ls!()
      |> Enum.filter(&String.ends_with?(&1, ".beam"))

    # Build a lookup of all available stdlib beams: module_name -> source_path
    stdlib_index =
      stdlib_beams
      |> Enum.map(fn f ->
        mod = f |> String.replace_trailing(".beam", "")
        {mod, Path.join(stdlib_dir, f)}
      end)
      |> Map.new()

    # Copy framework + user beams (these are always included)
    Enum.each(fw_beams, fn f ->
      File.cp!(Path.join(ew_ebin, f), Path.join(tmp_beams, f))
    end)

    Enum.each(user_beams, fn f ->
      File.cp!(Path.join(ebin_dir, f), Path.join(tmp_beams, f))
    end)

    # Tree-shake: only include stdlib modules transitively imported by user + framework code
    root_beams =
      (fw_beams ++ user_beams)
      |> Enum.map(fn f -> Path.join(tmp_beams, f) end)

    needed = resolve_stdlib_deps(root_beams, stdlib_index)

    Enum.each(needed, fn {_mod, src_path} ->
      File.cp!(src_path, Path.join(tmp_beams, Path.basename(src_path)))
    end)

    step("collecting", "stdlib (#{map_size(needed)}/#{length(stdlib_beams)} modules)")
    step("collecting", "framework (#{length(fw_beams)} modules)")

    # 4. Detect startup module: find the module that uses ElixirWorkers.App
    startup_beam = detect_startup_module()
    startup_name = String.replace_trailing(startup_beam, ".beam", "") |> String.replace_leading("Elixir.", "")

    step("packing", "app.avm (startup: #{startup_name})")

    # 5. Set up _build/worker/ with runtime artifacts
    setup_worker_dir(worker_dir, ew_priv)

    # 6. Ensure wrangler.jsonc + package.json exist at project root
    ensure_root_config(project_root, ew_priv, app)

    # 7. Install npm deps if needed
    unless File.dir?(Path.join(project_root, "node_modules")) do
      step("installing", "npm dependencies")
      System.cmd("npm", ["install"], cd: project_root)
    end

    # 8. Pack .avm
    avm_path = Path.join(worker_dir, "app.avm")

    {total_size, num_modules} =
      ElixirWorkers.Packer.create_avm(avm_path, startup_beam, tmp_beams)

    # Cleanup
    File.rm_rf!(tmp_beams)

    elapsed = System.monotonic_time(:millisecond) - started
    size_kb = Float.round(total_size / 1024, 1)

    IO.puts("")
    IO.puts(
      "  #{IO.ANSI.green()}#{IO.ANSI.bright()}Built#{IO.ANSI.reset()} " <>
        "_build/worker/app.avm " <>
        "#{IO.ANSI.faint()}— #{num_modules} modules, #{size_kb} KB (#{elapsed}ms)#{IO.ANSI.reset()}"
    )
  end

  defp step(action, detail) do
    padded = String.pad_trailing(action, 12)

    IO.puts(
      "  #{IO.ANSI.magenta()}#{padded}#{IO.ANSI.reset()} #{detail}"
    )
  end

  defp detect_startup_module do
    lib_dir = Path.join(File.cwd!(), "lib")

    startup =
      if File.dir?(lib_dir) do
        lib_dir
        |> find_ex_files()
        |> Enum.find_value(fn path ->
          content = File.read!(path)

          if String.contains?(content, "use ElixirWorkers.App") do
            case Regex.run(~r/defmodule\s+([\w.]+)/, content) do
              [_, mod] -> "Elixir.#{mod}.beam"
              _ -> nil
            end
          end
        end)
      end

    startup || Mix.raise("No module found with `use ElixirWorkers.App`. Define your app entry point.")
  end

  defp find_ex_files(dir) do
    dir
    |> File.ls!()
    |> Enum.flat_map(fn entry ->
      path = Path.join(dir, entry)

      cond do
        File.dir?(path) -> find_ex_files(path)
        String.ends_with?(entry, ".ex") -> [path]
        true -> []
      end
    end)
  end

  defp setup_worker_dir(worker_dir, ew_priv) do
    File.mkdir_p!(worker_dir)
    File.mkdir_p!(Path.join(worker_dir, "src"))

    templates_dir = Path.join(ew_priv, "templates")

    # Always overwrite index.js — it's a framework internal, not a user file
    index_src = Path.join(templates_dir, "index.js")
    index_dst = Path.join([worker_dir, "src", "index.js"])

    if File.exists?(index_src) do
      File.cp!(index_src, index_dst)
    end

    # Always overwrite atomvm.wasm — framework binary
    wasm_src = Path.join(ew_priv, "atomvm.wasm")
    wasm_dst = Path.join(worker_dir, "atomvm.wasm")

    if File.exists?(wasm_src) do
      File.cp!(wasm_src, wasm_dst)
    end
  end

  defp list_beams(dir) do
    if File.dir?(dir) do
      dir |> File.ls!() |> Enum.filter(&String.ends_with?(&1, ".beam"))
    else
      []
    end
  end

  # Walk imports transitively from root beams, returning only the stdlib modules needed.
  defp resolve_stdlib_deps(root_beam_paths, stdlib_index) do
    # Seed: extract imports from all root (user + framework) beams
    initial_imports =
      root_beam_paths
      |> Enum.reduce(MapSet.new(), fn path, acc ->
        data = File.read!(path)
        MapSet.union(acc, ElixirWorkers.Packer.imported_modules(data))
      end)

    walk_deps(initial_imports, stdlib_index, %{})
  end

  defp walk_deps(to_check, stdlib_index, found) do
    # Filter to stdlib modules we haven't visited yet
    new_mods =
      to_check
      |> Enum.filter(fn mod -> Map.has_key?(stdlib_index, mod) and not Map.has_key?(found, mod) end)

    if new_mods == [] do
      found
    else
      # Add these modules and extract their transitive imports
      {found, next_imports} =
        Enum.reduce(new_mods, {found, MapSet.new()}, fn mod, {f, imports} ->
          src_path = Map.fetch!(stdlib_index, mod)
          data = File.read!(src_path)
          mod_imports = ElixirWorkers.Packer.imported_modules(data)
          {Map.put(f, mod, src_path), MapSet.union(imports, mod_imports)}
        end)

      walk_deps(next_imports, stdlib_index, found)
    end
  end

  defp ensure_root_config(project_root, ew_priv, app) do
    templates_dir = Path.join(ew_priv, "templates")
    app_name = to_string(app) |> String.replace("_", "-")

    # Generate wrangler.jsonc at project root if missing
    wrangler_dst = Path.join(project_root, "wrangler.jsonc")

    unless File.exists?(wrangler_dst) do
      wrangler_tmpl = Path.join(templates_dir, "wrangler.jsonc.eex")

      if File.exists?(wrangler_tmpl) do
        content = EEx.eval_file(wrangler_tmpl, assigns: [app_name: app_name, port: 8797])
        File.write!(wrangler_dst, content)
        step("generated", "wrangler.jsonc")
      end
    end

    # Generate package.json at project root if missing
    pkg_dst = Path.join(project_root, "package.json")

    unless File.exists?(pkg_dst) do
      pkg_tmpl = Path.join(templates_dir, "package.json.eex")

      if File.exists?(pkg_tmpl) do
        content = EEx.eval_file(pkg_tmpl, assigns: [app_name: app_name])
        File.write!(pkg_dst, content)
        step("generated", "package.json")
      end
    end
  end
end
