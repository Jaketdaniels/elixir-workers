defmodule ElixirWorkers.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixir_workers,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: false,
      deps: deps(),
      # AtomVM-specific configuration
      atomvm: [
        start_module: ElixirWorkers,
        flash_offset: 0x210000
      ]
    ]
  end

  def application do
    [
      extra_applications: []
    ]
  end

  defp deps do
    []
  end
end
