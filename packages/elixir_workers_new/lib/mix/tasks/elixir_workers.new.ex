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
  - `worker/wrangler.jsonc` for Cloudflare config
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
    project_dir = Path.join(File.cwd!(), name)

    if File.dir?(project_dir) do
      Mix.raise("Directory #{name} already exists")
    end

    Mix.shell().info("Creating ElixirWorkers project #{name}...")

    # Create directories
    File.mkdir_p!(Path.join(project_dir, "lib/#{name}"))
    File.mkdir_p!(Path.join(project_dir, "worker"))

    templates_dir = templates_path()

    # Render templates
    assigns = [app_name: name, app_module: app_module]

    render_template(templates_dir, "mix.exs.eex", Path.join(project_dir, "mix.exs"), assigns)
    render_template(templates_dir, "app.ex.eex", Path.join(project_dir, "lib/#{name}.ex"), assigns)
    render_template(templates_dir, "router.ex.eex", Path.join(project_dir, "lib/#{name}/router.ex"), assigns)

    # Write .gitignore
    File.write!(Path.join(project_dir, ".gitignore"), gitignore_content())

    Mix.shell().info("""

    Your ElixirWorkers project is ready!

    Next steps:

        $ cd #{name}
        $ mix deps.get
        $ mix elixir_workers.dev

    Then visit http://localhost:8797 in your browser.
    """)
  end

  defp templates_path do
    # When installed as an archive, use Application.app_dir
    # When running from source, use priv/ relative to this file
    case :code.priv_dir(:elixir_workers_new) do
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
    Mix.shell().info("  * creating #{Path.relative_to_cwd(output_path)}")
  end

  defp gitignore_content do
    """
    # Build
    /_build/
    /deps/

    # Worker artifacts
    /worker/node_modules/
    /worker/app.avm
    /worker/atomvm.wasm
    /worker/src/index.js
    /worker/package.json

    # Mix
    *.ez
    .elixir_ls/
    """
  end
end
