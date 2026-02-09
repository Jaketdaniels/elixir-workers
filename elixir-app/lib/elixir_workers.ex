defmodule ElixirWorkers do
  @moduledoc """
  Entry point for Elixir on Cloudflare Workers.

  This module reads an HTTP request from stdin (JSON), routes it through
  the application, and writes the HTTP response to stdout (JSON).

  ## Protocol

  stdin receives:
      {"method":"GET","url":"/path","headers":{"host":"example.com"},"body":""}

  stdout must produce:
      {"status":200,"headers":{"content-type":"text/html"},"body":"Hello!"}
  """

  def start() do
    request = read_request()
    response = ElixirWorkers.Router.handle(request)
    write_response(response)
  end

  defp read_request do
    case AtomVM.Wasi.read_stdin() do
      :undefined ->
        %{method: "GET", url: "/", headers: %{}, body: ""}

      data when is_binary(data) ->
        ElixirWorkers.JSON.decode(data)
    end
  end

  defp write_response(response) do
    json = ElixirWorkers.JSON.encode(response)
    AtomVM.Wasi.write_stdout(json)
  end
end
