defmodule ElixirWorkers.Wasi do
  @moduledoc false

  defdelegate read_stdin, to: AtomVM.Wasi
  defdelegate write_stdout(data), to: AtomVM.Wasi
end
