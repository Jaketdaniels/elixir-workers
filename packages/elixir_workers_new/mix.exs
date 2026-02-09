defmodule ElixirWorkersNew.MixProject do
  use Mix.Project

  @version "0.1.0"
  @source_url "https://github.com/example/elixir-workers"

  def project do
    [
      app: :elixir_workers_new,
      version: @version,
      elixir: "~> 1.17",
      start_permanent: false,
      deps: deps(),
      package: package(),
      description: "Project generator for ElixirWorkers",
      source_url: @source_url
    ]
  end

  def application do
    [extra_applications: [:eex]]
  end

  defp deps do
    []
  end

  defp package do
    [
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => @source_url},
      files: ["lib", "priv/templates", "mix.exs"]
    ]
  end
end
