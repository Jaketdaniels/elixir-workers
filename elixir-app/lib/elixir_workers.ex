defmodule ElixirWorkers do
  def start() do
    request = read_request()
    response = ElixirWorkers.Router.handle(request)
    write_response(response)
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
