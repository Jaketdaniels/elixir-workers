defmodule ElixirWorkers.JSON do
  @moduledoc """
  Minimal JSON encoder/decoder for AtomVM.

  AtomVM doesn't have Jason/Poison available, so we implement a minimal
  JSON parser that handles the HTTP request/response protocol.

  Supports: strings, integers, floats, booleans, null, objects, arrays.
  """

  # ---- Decoder ----

  def decode(binary) when is_binary(binary) do
    {value, _rest} = decode_value(binary, 0)
    value
  end

  defp decode_value(bin, pos) do
    pos = skip_whitespace(bin, pos)

    case :binary.at(bin, pos) do
      ?{ -> decode_object(bin, pos + 1)
      ?[ -> decode_array(bin, pos + 1)
      ?" -> decode_string(bin, pos + 1)
      ?t -> {true, pos + 4}
      ?f -> {false, pos + 5}
      ?n -> {nil, pos + 4}
      c when c in [?-, ?0, ?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9] -> decode_number(bin, pos)
    end
  end

  defp decode_object(bin, pos) do
    pos = skip_whitespace(bin, pos)

    case :binary.at(bin, pos) do
      ?} ->
        {%{}, pos + 1}

      _ ->
        decode_object_pairs(bin, pos, %{})
    end
  end

  defp decode_object_pairs(bin, pos, acc) do
    pos = skip_whitespace(bin, pos)
    {key, pos} = decode_string(bin, pos + 1)
    pos = skip_whitespace(bin, pos)
    # skip colon
    pos = pos + 1
    {value, pos} = decode_value(bin, pos)
    acc = Map.put(acc, key, value)
    pos = skip_whitespace(bin, pos)

    case :binary.at(bin, pos) do
      ?, -> decode_object_pairs(bin, pos + 1, acc)
      ?} -> {acc, pos + 1}
    end
  end

  defp decode_array(bin, pos) do
    pos = skip_whitespace(bin, pos)

    case :binary.at(bin, pos) do
      ?] ->
        {[], pos + 1}

      _ ->
        decode_array_items(bin, pos, [])
    end
  end

  defp decode_array_items(bin, pos, acc) do
    {value, pos} = decode_value(bin, pos)
    acc = [value | acc]
    pos = skip_whitespace(bin, pos)

    case :binary.at(bin, pos) do
      ?, -> decode_array_items(bin, pos + 1, acc)
      ?] -> {:lists.reverse(acc), pos + 1}
    end
  end

  defp decode_string(bin, pos) do
    decode_string_chars(bin, pos, [])
  end

  defp decode_string_chars(bin, pos, acc) do
    case :binary.at(bin, pos) do
      ?" ->
        {:erlang.list_to_binary(:lists.reverse(acc)), pos + 1}

      ?\\ ->
        {char, pos} = decode_escape(bin, pos + 1)
        decode_string_chars(bin, pos, [char | acc])

      c ->
        decode_string_chars(bin, pos + 1, [c | acc])
    end
  end

  defp decode_escape(bin, pos) do
    case :binary.at(bin, pos) do
      ?" -> {?", pos + 1}
      ?\\ -> {?\\, pos + 1}
      ?/ -> {?/, pos + 1}
      ?n -> {?\n, pos + 1}
      ?r -> {?\r, pos + 1}
      ?t -> {?\t, pos + 1}
      ?b -> {?\b, pos + 1}
      ?f -> {?\f, pos + 1}
    end
  end

  defp decode_number(bin, pos) do
    {num_str, end_pos} = collect_number_chars(bin, pos, [])
    str = :erlang.list_to_binary(num_str)

    value =
      if :binary.match(str, ".") != :nomatch or :binary.match(str, "e") != :nomatch or
           :binary.match(str, "E") != :nomatch do
        :erlang.binary_to_float(str)
      else
        :erlang.binary_to_integer(str)
      end

    {value, end_pos}
  end

  defp collect_number_chars(bin, pos, acc) do
    if pos >= byte_size(bin) do
      {:lists.reverse(acc), pos}
    else
      c = :binary.at(bin, pos)

      if c in [?0, ?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?., ?-, ?+, ?e, ?E] do
        collect_number_chars(bin, pos + 1, [c | acc])
      else
        {:lists.reverse(acc), pos}
      end
    end
  end

  defp skip_whitespace(bin, pos) do
    if pos < byte_size(bin) do
      case :binary.at(bin, pos) do
        c when c in [?\s, ?\t, ?\n, ?\r] -> skip_whitespace(bin, pos + 1)
        _ -> pos
      end
    else
      pos
    end
  end

  # ---- Encoder ----

  def encode(value) do
    :erlang.list_to_binary(encode_value(value))
  end

  defp encode_value(nil), do: ~c"null"
  defp encode_value(true), do: ~c"true"
  defp encode_value(false), do: ~c"false"

  defp encode_value(n) when is_integer(n) do
    :erlang.integer_to_list(n)
  end

  defp encode_value(f) when is_float(f) do
    :erlang.float_to_list(f, [{:decimals, 6}, :compact])
  end

  defp encode_value(s) when is_binary(s) do
    [?" | encode_string_chars(s, 0, [])] ++ [?"]
  end

  defp encode_value(s) when is_atom(s) do
    encode_value(:erlang.atom_to_binary(s))
  end

  defp encode_value(list) when is_list(list) do
    [?[ | encode_array_items(list, [])]
  end

  defp encode_value(map) when is_map(map) do
    pairs = :maps.to_list(map)
    [?{ | encode_object_pairs(pairs, [])]
  end

  defp encode_string_chars(bin, pos, acc) do
    if pos >= byte_size(bin) do
      :lists.reverse(acc)
    else
      c = :binary.at(bin, pos)

      escaped =
        case c do
          ?" -> [?", ?\\]
          ?\\ -> [?\\, ?\\]
          ?\n -> [?n, ?\\]
          ?\r -> [?r, ?\\]
          ?\t -> [?t, ?\\]
          _ -> [c]
        end

      encode_string_chars(bin, pos + 1, escaped ++ acc)
    end
  end

  defp encode_array_items([], _), do: ~c"]"

  defp encode_array_items([item], _) do
    encode_value(item) ++ ~c"]"
  end

  defp encode_array_items([item | rest], _) do
    encode_value(item) ++ [?, | encode_array_items(rest, [])]
  end

  defp encode_object_pairs([], _), do: ~c"}"

  defp encode_object_pairs([{k, v}], _) do
    encode_value(k) ++ [?: | encode_value(v)] ++ ~c"}"
  end

  defp encode_object_pairs([{k, v} | rest], _) do
    encode_value(k) ++ [?: | encode_value(v)] ++ [?, | encode_object_pairs(rest, [])]
  end
end
