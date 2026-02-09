defmodule ElixirWorkers.JSON do
  # Minimal JSON encoder/decoder for AtomVM.
  # Supports: strings, integers, floats, booleans, null, objects, arrays.

  # ---- Decoder ----

  def decode(binary) when is_binary(binary) do
    {value, _rest} = decode_value(binary, 0)
    value
  end

  defp decode_value(bin, pos) do
    pos = skip_ws(bin, pos)

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
    pos = skip_ws(bin, pos)

    case :binary.at(bin, pos) do
      ?} -> {%{}, pos + 1}
      _ -> decode_pairs(bin, pos, %{})
    end
  end

  defp decode_pairs(bin, pos, acc) do
    pos = skip_ws(bin, pos)
    {key, pos} = decode_string(bin, pos + 1)
    pos = skip_ws(bin, pos)
    pos = pos + 1
    {value, pos} = decode_value(bin, pos)
    acc = :maps.put(key, value, acc)
    pos = skip_ws(bin, pos)

    case :binary.at(bin, pos) do
      ?, -> decode_pairs(bin, pos + 1, acc)
      ?} -> {acc, pos + 1}
    end
  end

  defp decode_array(bin, pos) do
    pos = skip_ws(bin, pos)

    case :binary.at(bin, pos) do
      ?] -> {[], pos + 1}
      _ -> decode_items(bin, pos, [])
    end
  end

  defp decode_items(bin, pos, acc) do
    {value, pos} = decode_value(bin, pos)
    acc = [value | acc]
    pos = skip_ws(bin, pos)

    case :binary.at(bin, pos) do
      ?, -> decode_items(bin, pos + 1, acc)
      ?] -> {:lists.reverse(acc), pos + 1}
    end
  end

  defp decode_string(bin, pos), do: dec_str(bin, pos, [])

  defp dec_str(bin, pos, acc) do
    case :binary.at(bin, pos) do
      ?" -> {:erlang.list_to_binary(:lists.reverse(acc)), pos + 1}
      ?\\ ->
        {char, pos} = dec_esc(bin, pos + 1)
        dec_str(bin, pos, [char | acc])
      c -> dec_str(bin, pos + 1, [c | acc])
    end
  end

  defp dec_esc(bin, pos) do
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

  # Collect number chars and track if it's a float in one pass
  defp decode_number(bin, pos) do
    {chars, end_pos, is_float} = collect_num(bin, pos, [], false)
    str = :erlang.list_to_binary(chars)

    value =
      if is_float do
        :erlang.binary_to_float(str)
      else
        :erlang.binary_to_integer(str)
      end

    {value, end_pos}
  end

  defp collect_num(bin, pos, acc, is_float) do
    if pos >= byte_size(bin) do
      {:lists.reverse(acc), pos, is_float}
    else
      c = :binary.at(bin, pos)

      case c do
        c when c in [?0, ?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?-, ?+] ->
          collect_num(bin, pos + 1, [c | acc], is_float)

        c when c in [?., ?e, ?E] ->
          collect_num(bin, pos + 1, [c | acc], true)

        _ ->
          {:lists.reverse(acc), pos, is_float}
      end
    end
  end

  defp skip_ws(bin, pos) do
    if pos < byte_size(bin) do
      case :binary.at(bin, pos) do
        c when c in [?\s, ?\t, ?\n, ?\r] -> skip_ws(bin, pos + 1)
        _ -> pos
      end
    else
      pos
    end
  end

  # ---- Encoder ----

  def encode(value) do
    :erlang.list_to_binary(enc_val(value))
  end

  defp enc_val(nil), do: ~c"null"
  defp enc_val(true), do: ~c"true"
  defp enc_val(false), do: ~c"false"

  defp enc_val(n) when is_integer(n) do
    :erlang.integer_to_list(n)
  end

  defp enc_val(f) when is_float(f) do
    :erlang.float_to_list(f, [{:decimals, 6}, :compact])
  end

  defp enc_val(s) when is_binary(s) do
    [?" | enc_str(s, 0, [])] ++ [?"]
  end

  defp enc_val(s) when is_atom(s) do
    enc_val(:erlang.atom_to_binary(s))
  end

  defp enc_val(list) when is_list(list) do
    [?[ | enc_arr(list)]
  end

  defp enc_val(map) when is_map(map) do
    [?{ | enc_obj(:maps.to_list(map))]
  end

  defp enc_str(bin, pos, acc) do
    if pos >= byte_size(bin) do
      :lists.reverse(acc)
    else
      c = :binary.at(bin, pos)

      case c do
        ?" -> enc_str(bin, pos + 1, [?", ?\\ | acc])
        ?\\ -> enc_str(bin, pos + 1, [?\\, ?\\ | acc])
        ?\n -> enc_str(bin, pos + 1, [?n, ?\\ | acc])
        ?\r -> enc_str(bin, pos + 1, [?r, ?\\ | acc])
        ?\t -> enc_str(bin, pos + 1, [?t, ?\\ | acc])
        _ -> enc_str(bin, pos + 1, [c | acc])
      end
    end
  end

  defp enc_arr([]), do: ~c"]"
  defp enc_arr([item]), do: enc_val(item) ++ ~c"]"
  defp enc_arr([item | rest]), do: enc_val(item) ++ [?, | enc_arr(rest)]

  defp enc_obj([]), do: ~c"}"
  defp enc_obj([{k, v}]), do: enc_val(k) ++ [?: | enc_val(v)] ++ ~c"}"
  defp enc_obj([{k, v} | rest]), do: enc_val(k) ++ [?: | enc_val(v)] ++ [?, | enc_obj(rest)]
end
