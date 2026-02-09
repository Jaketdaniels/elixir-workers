defmodule ElixirWorkers.MixProject do
  use Mix.Project

  @version "0.1.0"
  @source_url "https://github.com/Jaketdaniels/elixir-workers"

  def project do
    [
      app: :elixir_workers,
      version: @version,
      elixir: "~> 1.17",
      start_permanent: false,
      deps: deps(),
      package: package(),
      description: "Run Elixir on Cloudflare Workers via AtomVM compiled to WebAssembly",
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
      files: [
        "lib",
        "priv/atomvm.wasm",
        "priv/stdlib",
        "priv/templates",
        "mix.exs",
        "README.md",
        "LICENSE",
        "NOTICE",
        "THIRD_PARTY_LICENSES"
      ]
    ]
  end
end
