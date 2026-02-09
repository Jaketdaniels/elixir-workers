defmodule Mix.Tasks.ElixirWorkers.Dev do
  use Mix.Task

  @shortdoc "Build and start local dev server with wrangler"

  @moduledoc """
  Builds the .avm archive and starts a local development server
  using Cloudflare's wrangler CLI.

      $ mix elixir_workers.dev

  This runs `mix elixir_workers.build` then starts `wrangler dev`
  from the project root (where `wrangler.jsonc` lives).
  """

  @impl Mix.Task
  def run(_args) do
    Mix.Task.run("elixir_workers.build")

    project_root = File.cwd!()

    IO.puts("")
    IO.puts("  #{IO.ANSI.magenta()}#{IO.ANSI.bright()}elixir-workers#{IO.ANSI.reset()} #{IO.ANSI.faint()}dev#{IO.ANSI.reset()}")
    IO.puts("  #{IO.ANSI.cyan()}http://localhost:8797#{IO.ANSI.reset()}")
    IO.puts("")

    port = Port.open({:spawn_executable, System.find_executable("npx")}, [
      :binary,
      :exit_status,
      :use_stdio,
      :stderr_to_stdout,
      args: ["wrangler", "dev"],
      cd: project_root
    ])

    stream_port(port)
  end

  defp stream_port(port) do
    receive do
      {^port, {:data, data}} ->
        IO.write(data)
        stream_port(port)

      {^port, {:exit_status, status}} ->
        if status != 0 do
          Mix.raise("wrangler dev exited with status #{status}")
        end
    end
  end
end
