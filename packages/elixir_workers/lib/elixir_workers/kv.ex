defmodule ElixirWorkers.KV do
  @moduledoc false

  alias ElixirWorkers.Conn

  # --- Reads (transparent two-pass) ---

  # Get a value from KV.
  # Pass 1 (no bindings): registers a need, returns {conn, nil}.
  # Pass 2 (bindings present): reads from bindings, returns {conn, value | nil}.
  def get(conn, namespace, key) do
    need_id = need_id("kv_get", namespace, key)

    case Map.get(conn["bindings"], need_id) do
      nil ->
        # Not yet fulfilled — register the need
        need = %{"type" => "kv_get", "ns" => namespace, "key" => key, "id" => need_id}
        {Conn.add_need(conn, need), nil}

      value ->
        {conn, value}
    end
  end

  # Get a value with metadata from KV.
  def get_with_metadata(conn, namespace, key) do
    need_id = need_id("kv_get_meta", namespace, key)

    case Map.get(conn["bindings"], need_id) do
      nil ->
        need = %{"type" => "kv_get_meta", "ns" => namespace, "key" => key, "id" => need_id}
        {Conn.add_need(conn, need), nil}

      result ->
        {conn, result}
    end
  end

  # List keys in a KV namespace.
  # opts: %{"prefix" => "user:", "limit" => 100, "cursor" => "..."}
  def list(conn, namespace, opts \\ %{}) do
    prefix = Map.get(opts, "prefix", "")
    need_id = need_id("kv_list", namespace, prefix)

    case Map.get(conn["bindings"], need_id) do
      nil ->
        need = %{"type" => "kv_list", "ns" => namespace, "id" => need_id}
        need = if prefix != "", do: Map.put(need, "prefix", prefix), else: need
        need = case Map.get(opts, "limit") do
          nil -> need
          limit -> Map.put(need, "limit", limit)
        end
        need = case Map.get(opts, "cursor") do
          nil -> need
          cursor -> Map.put(need, "cursor", cursor)
        end
        {Conn.add_need(conn, need), nil}

      result ->
        {conn, result}
    end
  end

  # --- Writes (effects, executed post-response) ---

  # Put a value into KV.
  # opts: %{"expiration_ttl" => 3600, "metadata" => %{...}}
  def put(conn, namespace, key, value, opts \\ %{}) do
    effect = %{"type" => "kv_put", "ns" => namespace, "key" => key, "value" => value}

    effect = case Map.get(opts, "expiration_ttl") do
      nil -> effect
      ttl -> Map.put(effect, "expiration_ttl", ttl)
    end

    effect = case Map.get(opts, "metadata") do
      nil -> effect
      meta -> Map.put(effect, "metadata", meta)
    end

    Conn.add_effect(conn, effect)
  end

  # Delete a key from KV.
  def delete(conn, namespace, key) do
    Conn.add_effect(conn, %{"type" => "kv_delete", "ns" => namespace, "key" => key})
  end

  # --- Internal ---

  defp need_id(type, namespace, key) do
    type <> ":" <> namespace <> ":" <> key
  end
end
