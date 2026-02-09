defmodule Mix.Tasks.ElixirWorkers.Deploy do
  use Mix.Task

  @shortdoc "Build and deploy to Cloudflare Workers"

  @moduledoc """
  Builds the .avm archive and deploys to Cloudflare Workers
  using wrangler.

      $ mix elixir_workers.deploy

  This runs `mix elixir_workers.build` then `wrangler deploy`
  from the project root (where `wrangler.jsonc` lives).
  """

  @impl Mix.Task
  def run(_args) do
    Mix.Task.run("elixir_workers.build")

    project_root = File.cwd!()

    IO.puts("")
    IO.puts("  #{IO.ANSI.magenta()}#{IO.ANSI.bright()}Deploying#{IO.ANSI.reset()} to Cloudflare Workers...")

    {_output, status} = System.cmd("npx", ["wrangler", "deploy"], cd: project_root, into: IO.stream())

    if status != 0 do
      Mix.raise("wrangler deploy failed with status #{status}")
    end
  end
end
