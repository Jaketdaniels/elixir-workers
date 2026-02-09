defmodule ElixirWorkers.Router do
  @moduledoc false

  alias ElixirWorkers.Conn
  alias ElixirWorkers.Middleware
  alias ElixirWorkers.URL

  # Entry point: run middleware, then dispatch to a matching route.
  def call(conn) do
    conn = Middleware.run(conn, middleware())

    if conn["halted"] do
      conn
    else
      dispatch(conn)
    end
  end

  # --- Middleware Pipeline ---
  # Customize this list to add/remove middleware for all routes.

  defp middleware do
    [
      &Middleware.security_headers/1,
      &Middleware.parse_body/1
    ]
  end

  # --- Route Table ---
  # Each entry: {method, pattern_segments, handler_fun}
  # Pattern segments starting with ":" capture path params.
  # Method "*" matches any method.

  defp routes do
    [
      {"GET", [], &page_home/1},
      {"GET", ["api", "health"], &api_health/1},
      {"POST", ["api", "echo"], &api_echo/1}
    ]
  end

  # --- Dispatcher ---

  defp dispatch(conn) do
    method = conn["method"]
    segments = conn["path_segments"]

    case find_route(routes(), method, segments) do
      {:ok, handler, params} ->
        conn = Map.put(conn, "path_params", params)
        handler.(conn)

      :no_match ->
        not_found(conn)
    end
  end

  defp find_route([], _method, _segments), do: :no_match

  defp find_route([{route_method, pattern, handler} | rest], method, segments) do
    if route_method == method or route_method == "*" do
      case URL.match_path(segments, pattern) do
        {:ok, params} -> {:ok, handler, params}
        :no_match -> find_route(rest, method, segments)
      end
    else
      find_route(rest, method, segments)
    end
  end

  # --- Built-in Handlers ---

  defp page_home(conn) do
    Conn.html(conn, 200,
      <<"<!DOCTYPE html><html><head><title>Elixir Workers</title></head><body>",
        "<h1>Running Elixir on Cloudflare Workers</h1>",
        "<p>Powered by AtomVM + WASI</p>",
        "</body></html>">>
    )
  end

  defp api_health(conn) do
    Conn.json(conn, 200, %{
      "status" => "ok",
      "runtime" => "atomvm-wasi",
      "platform" => "cloudflare-workers",
      "env" => conn["env"]
    })
  end

  defp api_echo(conn) do
    Conn.json(conn, 200, %{
      "echo" => conn["body"],
      "method" => conn["method"],
      "content_type" => Map.get(conn["headers"], "content-type", ""),
      "query_params" => conn["query_params"],
      "parsed_body" => conn["parsed_body"]
    })
  end

  defp not_found(conn) do
    Conn.json(conn, 404, %{
      "error" => "not_found",
      "method" => conn["method"],
      "path" => conn["path"]
    })
  end
end
