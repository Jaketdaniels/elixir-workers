defmodule ElixirWorkers.App do
  defmacro __using__(opts) do
    router = Keyword.fetch!(opts, :router)

    quote do
      def start() do
        raw = ElixirWorkers.read_request()
        conn = ElixirWorkers.Conn.new(raw)
        conn = unquote(router).call(conn)
        ElixirWorkers.write_response(ElixirWorkers.Conn.to_response(conn))
      end
    end
  end
end
