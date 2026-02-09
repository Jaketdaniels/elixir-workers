defmodule Mix.Tasks.ElixirWorkers.Deploy do
  use Mix.Task

  @shortdoc "Build and deploy to Cloudflare Workers"

  @moduledoc """
  Builds the .avm archive and deploys to Cloudflare Workers
  using wrangler.

      $ mix elixir_workers.deploy

  This runs `mix elixir_workers.build` then `wrangler deploy`
  in the `worker/` directory.
  """

  @impl Mix.Task
  def run(_args) do
    Mix.Task.run("elixir_workers.build")

    worker_dir = Path.join(File.cwd!(), "worker")

    Mix.shell().info("Deploying to Cloudflare Workers...")

    {_output, status} = System.cmd("npx", ["wrangler", "deploy"], cd: worker_dir, into: IO.stream())

    if status != 0 do
      Mix.raise("wrangler deploy failed with status #{status}")
    end
  end
end
