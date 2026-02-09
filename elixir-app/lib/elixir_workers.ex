defmodule ElixirWorkers do
  def start() do
    raw = read_request()
    conn = ElixirWorkers.Conn.new(raw)
    conn = ElixirWorkers.Router.call(conn)
    write_response(ElixirWorkers.Conn.to_response(conn))
  end

  defp read_request do
    case AtomVM.Wasi.read_stdin() do
      :undefined ->
        %{"method" => "GET", "url" => "/", "headers" => %{}, "body" => ""}

      data when is_binary(data) ->
        ElixirWorkers.JSON.decode(data)
    end
  end

  defp write_response(response) do
    json = ElixirWorkers.JSON.encode(response)
    AtomVM.Wasi.write_stdout(json)
  end
end
