defmodule ElixirWorkers.Conn do
  @moduledoc false

  # Build an enriched conn map from the raw request JSON.
  def new(raw) when is_map(raw) do
    url = Map.get(raw, "url", "/")
    parsed = ElixirWorkers.URL.parse_path(url)
    path = parsed["path"]

    %{
      "method" => Map.get(raw, "method", "GET"),
      "url" => url,
      "path" => path,
      "query_string" => parsed["query_string"],
      "query_params" => ElixirWorkers.URL.decode_query(parsed["query_string"]),
      "path_segments" => ElixirWorkers.URL.split_path(path),
      "path_params" => %{},
      "headers" => Map.get(raw, "headers", %{}),
      "body" => Map.get(raw, "body", ""),
      "parsed_body" => nil,
      "env" => Map.get(raw, "env", %{}),
      "cf" => Map.get(raw, "cf", %{}),
      "bindings" => Map.get(raw, "bindings", %{}),
      "_state" => Map.get(raw, "_state", %{}),
      "_needs" => [],
      "_effects" => [],
      "status" => nil,
      "resp_headers" => %{},
      "resp_body" => nil,
      "halted" => false
    }
  end

  # --- Response Builders ---

  def html(conn, status, body) do
    conn
    |> put_resp_header("content-type", "text/html; charset=utf-8")
    |> send_resp(status, body)
  end

  def json(conn, status, data) do
    conn
    |> put_resp_header("content-type", "application/json")
    |> send_resp(status, ElixirWorkers.JSON.encode(data))
  end

  def text(conn, status, body) do
    conn
    |> put_resp_header("content-type", "text/plain; charset=utf-8")
    |> send_resp(status, body)
  end

  def redirect(conn, url, status \\ 302) do
    conn
    |> put_resp_header("location", url)
    |> send_resp(status, "")
  end

  def put_resp_header(conn, key, value) do
    headers = Map.get(conn, "resp_headers", %{})
    Map.put(conn, "resp_headers", :maps.put(key, value, headers))
  end

  def send_resp(conn, status, body) do
    conn
    |> Map.put("status", status)
    |> Map.put("resp_body", body)
  end

  # --- Binding Helpers ---

  def add_need(conn, need) do
    Map.put(conn, "_needs", [need | conn["_needs"]])
  end

  def add_effect(conn, effect) do
    Map.put(conn, "_effects", [effect | conn["_effects"]])
  end

  def needs_bindings?(conn) do
    conn["_needs"] != []
  end

  # --- State (survives between passes) ---

  def put_state(conn, key, value) do
    state = Map.get(conn, "_state", %{})
    Map.put(conn, "_state", :maps.put(key, value, state))
  end

  def get_state(conn, key, default \\ nil) do
    state = Map.get(conn, "_state", %{})
    Map.get(state, key, default)
  end

  # --- Final Output ---

  # Build the response map to write to stdout.
  # If there are unfulfilled binding needs, returns a _needs response.
  # Otherwise returns the HTTP response, optionally with _effects.
  def to_response(conn) do
    if needs_bindings?(conn) do
      %{
        "_needs" => :lists.reverse(conn["_needs"]),
        "_state" => conn["_state"]
      }
    else
      resp = %{
        "status" => conn["status"] || 200,
        "headers" => conn["resp_headers"],
        "body" => conn["resp_body"] || ""
      }

      case conn["_effects"] do
        [] -> resp
        effects -> Map.put(resp, "_effects", :lists.reverse(effects))
      end
    end
  end
end
