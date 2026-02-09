defmodule ElixirWorkers.URL do
  @moduledoc false

  # Split a URL string into path and query_string.
  # "/posts/42?page=2&q=hi" -> %{"path" => "/posts/42", "query_string" => "page=2&q=hi"}
  def parse_path(url) when is_binary(url) do
    case find_char(url, ??, 0) do
      :none ->
        %{"path" => url, "query_string" => ""}

      pos ->
        %{
          "path" => :binary.part(url, 0, pos),
          "query_string" => :binary.part(url, pos + 1, byte_size(url) - pos - 1)
        }
    end
  end

  # Split path into segments: "/posts/42" -> ["posts", "42"]
  # Root "/" -> []
  def split_path(path) when is_binary(path) do
    split_on_slash(path, 0, 0, [])
  end

  # Decode a query string: "a=1&b=hello+world&c=%2F" -> %{"a" => "1", "b" => "hello world", "c" => "/"}
  def decode_query(""), do: %{}

  def decode_query(qs) when is_binary(qs) do
    decode_pairs(qs, 0, 0, %{})
  end

  # Percent-decode a binary: "hello%20world" -> "hello world"
  def percent_decode(bin) when is_binary(bin) do
    pct_dec(bin, 0, [])
  end

  # Match path segments against a pattern with :param captures.
  # match_path(["posts", "42"], ["posts", ":id"]) -> {:ok, %{"id" => "42"}}
  # match_path(["posts"], ["posts", ":id"]) -> :no_match
  def match_path(segments, pattern) do
    do_match(segments, pattern, %{})
  end

  # --- Internal: parse_path ---

  defp find_char(bin, char, pos) do
    if pos >= byte_size(bin) do
      :none
    else
      case :binary.at(bin, pos) do
        ^char -> pos
        _ -> find_char(bin, char, pos + 1)
      end
    end
  end

  # --- Internal: split_path ---

  defp split_on_slash(bin, pos, seg_start, acc) do
    if pos >= byte_size(bin) do
      seg_len = pos - seg_start
      if seg_len > 0 do
        :lists.reverse([:binary.part(bin, seg_start, seg_len) | acc])
      else
        :lists.reverse(acc)
      end
    else
      case :binary.at(bin, pos) do
        ?/ ->
          seg_len = pos - seg_start
          if seg_len > 0 do
            split_on_slash(bin, pos + 1, pos + 1, [:binary.part(bin, seg_start, seg_len) | acc])
          else
            split_on_slash(bin, pos + 1, pos + 1, acc)
          end

        _ ->
          split_on_slash(bin, pos + 1, seg_start, acc)
      end
    end
  end

  # --- Internal: decode_query ---

  defp decode_pairs(qs, pos, pair_start, acc) do
    if pos >= byte_size(qs) do
      add_pair(qs, pair_start, pos, acc)
    else
      case :binary.at(qs, pos) do
        ?& -> decode_pairs(qs, pos + 1, pos + 1, add_pair(qs, pair_start, pos, acc))
        _ -> decode_pairs(qs, pos + 1, pair_start, acc)
      end
    end
  end

  defp add_pair(_qs, start, stop, acc) when start >= stop, do: acc

  defp add_pair(qs, start, stop, acc) do
    pair = :binary.part(qs, start, stop - start)

    case find_char(pair, ?=, 0) do
      :none ->
        :maps.put(percent_decode(pair), "", acc)

      eq_pos ->
        key = percent_decode(:binary.part(pair, 0, eq_pos))
        val = percent_decode(:binary.part(pair, eq_pos + 1, byte_size(pair) - eq_pos - 1))
        :maps.put(key, val, acc)
    end
  end

  # --- Internal: percent_decode ---

  defp pct_dec(bin, pos, acc) do
    if pos >= byte_size(bin) do
      :erlang.list_to_binary(:lists.reverse(acc))
    else
      case :binary.at(bin, pos) do
        ?% when pos + 2 < byte_size(bin) ->
          hi = hex_digit(:binary.at(bin, pos + 1))
          lo = hex_digit(:binary.at(bin, pos + 2))

          if hi >= 0 and lo >= 0 do
            pct_dec(bin, pos + 3, [hi * 16 + lo | acc])
          else
            pct_dec(bin, pos + 1, [?% | acc])
          end

        ?+ ->
          pct_dec(bin, pos + 1, [?\s | acc])

        c ->
          pct_dec(bin, pos + 1, [c | acc])
      end
    end
  end

  defp hex_digit(c) when c >= ?0 and c <= ?9, do: c - ?0
  defp hex_digit(c) when c >= ?a and c <= ?f, do: c - ?a + 10
  defp hex_digit(c) when c >= ?A and c <= ?F, do: c - ?A + 10
  defp hex_digit(_), do: -1

  # --- Internal: match_path ---

  defp do_match([], [], params), do: {:ok, params}
  defp do_match(_, [], _), do: :no_match
  defp do_match([], _, _), do: :no_match

  defp do_match([seg | rest_segs], [pat | rest_pats], params) do
    case pat do
      <<":", name::binary>> ->
        do_match(rest_segs, rest_pats, :maps.put(name, seg, params))

      "*" ->
        # Wildcard matches remaining segments
        {:ok, :maps.put("*", Enum.join([seg | rest_segs], "/"), params)}

      ^seg ->
        do_match(rest_segs, rest_pats, params)

      _ ->
        :no_match
    end
  end
end
