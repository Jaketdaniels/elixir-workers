defmodule ElixirWorkers.Wasi do
  def read_stdin, do: :erlang.nif_error(:nif_not_loaded)
  def write_stdout(_data), do: :erlang.nif_error(:nif_not_loaded)
end
