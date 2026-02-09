defmodule ElixirWorkers.Router do
  @moduledoc false

  defmacro __using__(_opts) do
    quote do
      alias ElixirWorkers.{Conn, Middleware, URL}

      def call(conn) do
        conn = Middleware.run(conn, middleware())

        if conn["halted"] do
          conn
        else
          dispatch(conn)
        end
      end

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

      defp not_found(conn) do
        Conn.json(conn, 404, %{
          "error" => "not_found",
          "method" => conn["method"],
          "path" => conn["path"]
        })
      end

      # Default middleware -- user can override
      defp middleware do
        [
          &Middleware.security_headers/1,
          &Middleware.parse_body/1
        ]
      end

      defoverridable [middleware: 0]
    end
  end
end
