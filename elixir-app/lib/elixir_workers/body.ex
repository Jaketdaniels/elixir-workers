defmodule ElixirWorkers.Body do
  @moduledoc false

  # Parse a request body based on content-type header.
  def parse(body, content_type) when is_binary(body) and is_binary(content_type) do
    cond do
      starts_with(content_type, "application/x-www-form-urlencoded") ->
        parse_urlencoded(body)

      starts_with(content_type, "application/json") ->
        parse_json(body)

      true ->
        body
    end
  end

  def parse(body, _), do: body

  # Parse URL-encoded form body: "name=Alice&age=30" -> %{"name" => "Alice", "age" => "30"}
  def parse_urlencoded(body) when is_binary(body) do
    ElixirWorkers.URL.decode_query(body)
  end

  # Parse JSON body
  def parse_json(body) when is_binary(body) and byte_size(body) > 0 do
    ElixirWorkers.JSON.decode(body)
  end

  def parse_json(_), do: %{}

  # --- Internal ---

  defp starts_with(bin, prefix) do
    plen = byte_size(prefix)

    if byte_size(bin) >= plen do
      :binary.part(bin, 0, plen) == prefix
    else
      false
    end
  end
end
