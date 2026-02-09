defmodule Mix.Tasks.ElixirWorkers.New do
  use Mix.Task

  @shortdoc "Create a new ElixirWorkers project"

  @moduledoc """
  Creates a new ElixirWorkers project for running Elixir on Cloudflare Workers.

      $ mix elixir_workers.new my_app

  The project name must be a valid Elixir app name (lowercase, underscores).

  This generates:
  - `mix.exs` with `:elixir_workers` dependency
  - `lib/my_app.ex` with the app entry point
  - `lib/my_app/router.ex` with example routes
  - `wrangler.jsonc` for Cloudflare Workers config
  - `package.json` for wrangler npm dependency
  - `.gitignore`

  After generating, run:
      $ cd my_app
      $ mix deps.get
      $ mix elixir_workers.dev
  """

  @impl Mix.Task
  def run(args) do
    case args do
      [name | _] ->
        generate(name)

      [] ->
        Mix.raise("Usage: mix elixir_workers.new APP_NAME")
    end
  end

  defp generate(name) do
    unless name =~ ~r/^[a-z][a-z0-9_]*$/ do
      Mix.raise(
        "App name must be lowercase, start with a letter, and only contain letters, numbers, and underscores. Got: #{name}"
      )
    end

    app_module = Macro.camelize(name)
    app_name = String.replace(name, "_", "-")
    project_dir = Path.join(File.cwd!(), name)

    if File.dir?(project_dir) do
      Mix.raise("Directory #{name} already exists")
    end

    IO.puts("")
    IO.puts(
      "  #{IO.ANSI.magenta()}#{IO.ANSI.bright()}Creating#{IO.ANSI.reset()}  #{name}"
    )
    IO.puts("")

    # Create directories
    File.mkdir_p!(Path.join(project_dir, "lib/#{name}"))

    templates_dir = templates_path()

    # Render Elixir templates
    assigns = [app_name: name, app_module: app_module]

    render_template(templates_dir, "mix.exs.eex", Path.join(project_dir, "mix.exs"), assigns)
    render_template(templates_dir, "app.ex.eex", Path.join(project_dir, "lib/#{name}.ex"), assigns)
    render_template(templates_dir, "router.ex.eex", Path.join(project_dir, "lib/#{name}/router.ex"), assigns)

    # Render Workers config at project root
    worker_assigns = [app_name: app_name, port: 8797]
    render_template(templates_dir, "wrangler.jsonc.eex", Path.join(project_dir, "wrangler.jsonc"), worker_assigns)
    render_template(templates_dir, "package.json.eex", Path.join(project_dir, "package.json"), worker_assigns)

    # Write .gitignore
    File.write!(Path.join(project_dir, ".gitignore"), gitignore_content())
    created(".gitignore")

    IO.puts("")
    IO.puts("  #{IO.ANSI.green()}#{IO.ANSI.bright()}Your project is ready!#{IO.ANSI.reset()}")
    IO.puts("")
    IO.puts("  #{IO.ANSI.faint()}Next steps:#{IO.ANSI.reset()}")
    IO.puts("")
    IO.puts("      cd #{name}")
    IO.puts("      mix deps.get")
    IO.puts("      mix elixir_workers.dev")
    IO.puts("")
    IO.puts("  #{IO.ANSI.faint()}Then visit http://localhost:8797#{IO.ANSI.reset()}")
    IO.puts("")
  end

  defp templates_path do
    case :code.priv_dir(:elixir_workers) do
      {:error, _} ->
        Path.join([__DIR__, "..", "..", "..", "..", "priv", "templates"])
        |> Path.expand()

      priv_dir ->
        Path.join(to_string(priv_dir), "templates")
    end
  end

  defp render_template(templates_dir, template, output_path, assigns) do
    tmpl_path = Path.join(templates_dir, template)
    content = EEx.eval_file(tmpl_path, assigns: assigns)
    File.write!(output_path, content)
    created(Path.relative_to_cwd(output_path))
  end

  defp created(path) do
    IO.puts("  #{IO.ANSI.green()}+#{IO.ANSI.reset()} #{path}")
  end

  defp gitignore_content do
    """
    /_build/
    /deps/
    /node_modules/
    *.ez
    .elixir_ls/
    """
  end
end
