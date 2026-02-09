defmodule AtomVM.Wasi do
  @moduledoc false

  # This module name must match the NIF registration in atomvm-wasi/src/platform_nifs.c
  # which registers functions under "Elixir.AtomVM.Wasi:*".

  def read_stdin, do: :erlang.nif_error(:nif_not_loaded)
  def write_stdout(_data), do: :erlang.nif_error(:nif_not_loaded)
end
