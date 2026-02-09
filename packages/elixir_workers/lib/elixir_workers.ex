defmodule ElixirWorkers do
  def read_request do
    case ElixirWorkers.Wasi.read_stdin() do
      :undefined ->
        %{"method" => "GET", "url" => "/", "headers" => %{}, "body" => ""}

      data when is_binary(data) ->
        ElixirWorkers.JSON.decode(data)
    end
  end

  def write_response(response) do
    json = ElixirWorkers.JSON.encode(response)
    ElixirWorkers.Wasi.write_stdout(json)
  end
end
