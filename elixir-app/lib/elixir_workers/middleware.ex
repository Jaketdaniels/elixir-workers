defmodule ElixirWorkers.Middleware do
  @moduledoc false

  alias ElixirWorkers.Conn

  # Add standard security headers.
  def security_headers(conn) do
    conn
    |> Conn.put_resp_header("x-content-type-options", "nosniff")
    |> Conn.put_resp_header("x-frame-options", "DENY")
    |> Conn.put_resp_header("strict-transport-security", "max-age=31536000; includeSubDomains")
  end

  # Parse request body based on content-type.
  # Sets "parsed_body" on the conn.
  def parse_body(conn) do
    ct = Map.get(conn["headers"], "content-type", "")
    parsed = ElixirWorkers.Body.parse(conn["body"], ct)
    Map.put(conn, "parsed_body", parsed)
  end

  # CORS middleware. Handles OPTIONS preflight and adds headers.
  # opts: %{"origin" => "*", "methods" => "GET,POST,PUT,DELETE", "headers" => "content-type"}
  def cors(conn, opts \\ %{}) do
    origin = Map.get(opts, "origin", "*")
    methods = Map.get(opts, "methods", "GET,POST,PUT,DELETE,OPTIONS")
    headers = Map.get(opts, "headers", "content-type,authorization")
    max_age = Map.get(opts, "max_age", "86400")

    conn = conn
    |> Conn.put_resp_header("access-control-allow-origin", origin)
    |> Conn.put_resp_header("access-control-allow-methods", methods)
    |> Conn.put_resp_header("access-control-allow-headers", headers)

    # Handle OPTIONS preflight: respond immediately and halt
    if conn["method"] == "OPTIONS" do
      conn
      |> Conn.put_resp_header("access-control-max-age", max_age)
      |> Conn.send_resp(204, "")
      |> Map.put("halted", true)
    else
      conn
    end
  end

  # Run a list of middleware functions on a conn.
  # Stops early if any middleware sets "halted" to true.
  def run(conn, []), do: conn

  def run(conn, [mid | rest]) do
    if conn["halted"] do
      conn
    else
      run(mid.(conn), rest)
    end
  end
end
