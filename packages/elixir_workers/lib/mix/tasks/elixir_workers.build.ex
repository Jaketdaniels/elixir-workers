defmodule Mix.Tasks.ElixirWorkers.Build do
  use Mix.Task

  @shortdoc "Compile Elixir code and pack into .avm for Cloudflare Workers"

  @moduledoc """
  Compiles your Elixir project and packages it into an .avm archive
  that runs on AtomVM inside Cloudflare Workers.

      $ mix elixir_workers.build

  This task:
  1. Compiles your Elixir code with `mix compile`
  2. Collects user .beam files and pre-compiled stdlib .beam files
  3. Packs everything into `worker/app.avm`
  4. Sets up the `worker/` directory with JS runtime, wrangler config, etc.
  """

  @impl Mix.Task
  def run(_args) do
    # 1. Compile user code
    Mix.Task.run("compile", ["--no-deps-check"])

    app = Mix.Project.config()[:app]
    build_path = Mix.Project.build_path()
    worker_dir = Path.join(Mix.Project.app_path() |> Path.dirname() |> Path.dirname() |> Path.dirname(), "worker")

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

    # Copy stdlib beams
    stdlib_dir = Path.join(ew_priv, "stdlib")

    if File.dir?(stdlib_dir) do
      stdlib_dir
      |> File.ls!()
      |> Enum.filter(&String.ends_with?(&1, ".beam"))
      |> Enum.each(fn f ->
        File.cp!(Path.join(stdlib_dir, f), Path.join(tmp_beams, f))
      end)
    end

    # Copy user beams (overwrite stdlib if same name)
    ebin_dir
    |> File.ls!()
    |> Enum.filter(&String.ends_with?(&1, ".beam"))
    |> Enum.each(fn f ->
      File.cp!(Path.join(ebin_dir, f), Path.join(tmp_beams, f))
    end)

    # 4. Detect startup module: find the module that uses ElixirWorkers.App
    startup_beam = detect_startup_module(ebin_dir)

    Mix.shell().info("Packing .avm archive...")
    Mix.shell().info("  Startup module: #{startup_beam}")

    # 5. Set up worker/ directory
    setup_worker_dir(worker_dir, ew_priv, app)

    # 6. Pack .avm
    avm_path = Path.join(worker_dir, "app.avm")

    {total_size, num_modules} =
      ElixirWorkers.Packer.create_avm(avm_path, startup_beam, tmp_beams)

    Mix.shell().info("Created #{avm_path} (#{total_size} bytes, #{num_modules} modules)")

    # Cleanup
    File.rm_rf!(tmp_beams)
  end

  defp detect_startup_module(_ebin_dir) do
    # Look through source files for `use ElixirWorkers.App`
    lib_dir = Path.join(Mix.Project.app_path() |> Path.dirname() |> Path.dirname() |> Path.dirname(), "lib")

    startup =
      if File.dir?(lib_dir) do
        lib_dir
        |> find_ex_files()
        |> Enum.find_value(fn path ->
          content = File.read!(path)

          if String.contains?(content, "use ElixirWorkers.App") do
            # Extract module name from defmodule
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

  defp setup_worker_dir(worker_dir, ew_priv, app) do
    File.mkdir_p!(worker_dir)
    File.mkdir_p!(Path.join(worker_dir, "src"))

    templates_dir = Path.join(ew_priv, "templates")
    app_name = to_string(app) |> String.replace("_", "-")

    # Copy index.js (static, no templating)
    index_src = Path.join(templates_dir, "index.js")
    index_dst = Path.join([worker_dir, "src", "index.js"])

    unless File.exists?(index_dst) do
      if File.exists?(index_src) do
        File.cp!(index_src, index_dst)
      end
    end

    # Copy atomvm.wasm
    wasm_src = Path.join(ew_priv, "atomvm.wasm")
    wasm_dst = Path.join(worker_dir, "atomvm.wasm")

    unless File.exists?(wasm_dst) do
      if File.exists?(wasm_src) do
        File.cp!(wasm_src, wasm_dst)
      end
    end

    # Generate wrangler.jsonc from template
    wrangler_dst = Path.join(worker_dir, "wrangler.jsonc")

    unless File.exists?(wrangler_dst) do
      wrangler_tmpl = Path.join(templates_dir, "wrangler.jsonc.eex")

      if File.exists?(wrangler_tmpl) do
        content = EEx.eval_file(wrangler_tmpl, assigns: [app_name: app_name, port: 8797])
        File.write!(wrangler_dst, content)
      end
    end

    # Generate package.json from template
    pkg_dst = Path.join(worker_dir, "package.json")

    unless File.exists?(pkg_dst) do
      pkg_tmpl = Path.join(templates_dir, "package.json.eex")

      if File.exists?(pkg_tmpl) do
        content = EEx.eval_file(pkg_tmpl, assigns: [app_name: app_name])
        File.write!(pkg_dst, content)
      end
    end

    # Install npm deps if needed
    unless File.dir?(Path.join(worker_dir, "node_modules")) do
      Mix.shell().info("Installing npm dependencies in worker/...")
      System.cmd("npm", ["install"], cd: worker_dir)
    end
  end
end
