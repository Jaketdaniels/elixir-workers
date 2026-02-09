defmodule AtomVM.Wasi do
  @moduledoc """
  NIF bindings for WASI platform functions.

  These functions are implemented in C in `platform_nifs.c` and provide
  stdin/stdout access for the HTTP request/response bridge.
  """

  @doc "Read all available data from stdin. Returns binary or :undefined."
  def read_stdin do
    :erlang.nif_error(:nif_not_loaded)
  end

  @doc "Write binary data to stdout. Returns :ok or :error."
  def write_stdout(_data) do
    :erlang.nif_error(:nif_not_loaded)
  end
end
