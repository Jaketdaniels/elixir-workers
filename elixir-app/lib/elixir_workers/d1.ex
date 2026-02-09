defmodule ElixirWorkers.D1 do
  @moduledoc false

  alias ElixirWorkers.Conn

  # --- Reads (transparent two-pass) ---

  # Execute a SQL query and return all rows as a list of maps.
  # Pass 1: registers a need, returns {conn, nil}.
  # Pass 2: reads from bindings, returns {conn, [%{col => val}, ...] | nil}.
  def query(conn, database, sql, params \\ []) do
    need_id = need_id(database, sql, params)

    case Map.get(conn["bindings"], need_id) do
      nil ->
        need = %{"type" => "d1_query", "db" => database, "sql" => sql, "id" => need_id}
        need = if params != [], do: Map.put(need, "params", params), else: need
        {Conn.add_need(conn, need), nil}

      result when is_map(result) ->
        # Result contains "rows" (list of maps) from JS fulfillment
        {conn, Map.get(result, "rows", [])}

      result when is_list(result) ->
        {conn, result}
    end
  end

  # Execute a SQL query and return the first row only.
  def query_one(conn, database, sql, params \\ []) do
    case query(conn, database, sql, params) do
      {conn, nil} -> {conn, nil}
      {conn, []} -> {conn, nil}
      {conn, [first | _]} -> {conn, first}
    end
  end

  # --- Writes (effects, executed post-response) ---

  # Execute a write statement (INSERT/UPDATE/DELETE).
  def exec(conn, database, sql, params \\ []) do
    effect = %{"type" => "d1_exec", "db" => database, "sql" => sql}
    effect = if params != [], do: Map.put(effect, "params", params), else: effect
    Conn.add_effect(conn, effect)
  end

  # Execute multiple statements in a batch.
  # statements: [%{"sql" => "...", "params" => [...]}, ...]
  def batch(conn, database, statements) when is_list(statements) do
    Conn.add_effect(conn, %{
      "type" => "d1_batch",
      "db" => database,
      "statements" => statements
    })
  end

  # --- Internal ---

  defp need_id(database, sql, params) do
    param_str = Enum.map(params, &to_string/1) |> Enum.join(",")
    "d1:" <> database <> ":" <> sql <> ":" <> param_str
  end
end
