defmodule ElixirWorkers.HTML do
  @moduledoc false

  # Escape HTML entities to prevent XSS.
  # escape("<b>hi</b>") -> "&lt;b&gt;hi&lt;/b&gt;"
  def escape(bin) when is_binary(bin) do
    esc(bin, 0, [])
  end

  def escape(other), do: escape(to_string(other))

  # Build an HTML tag: tag("div", %{"class" => "card"}, "content") -> "<div class=\"card\">content</div>"
  def tag(name, attrs, content) when is_binary(name) do
    :erlang.iolist_to_binary([
      "<", name, encode_attrs(attrs), ">",
      content,
      "</", name, ">"
    ])
  end

  def tag(name, content) when is_binary(name) and is_binary(content) do
    tag(name, %{}, content)
  end

  # Build a void tag (self-closing): void_tag("input", %{"type" => "text"}) -> "<input type=\"text\"/>"
  def void_tag(name, attrs) when is_binary(name) do
    :erlang.iolist_to_binary(["<", name, encode_attrs(attrs), "/>"])
  end

  def void_tag(name) when is_binary(name), do: void_tag(name, %{})

  # Render a list by applying a function to each item and joining results.
  def each(list, render_fn) when is_list(list) do
    :erlang.iolist_to_binary(Enum.map(list, render_fn))
  end

  # --- Internal ---

  defp esc(bin, pos, acc) do
    if pos >= byte_size(bin) do
      :erlang.iolist_to_binary(:lists.reverse(acc))
    else
      case :binary.at(bin, pos) do
        ?& -> esc(bin, pos + 1, ["&amp;" | acc])
        ?< -> esc(bin, pos + 1, ["&lt;" | acc])
        ?> -> esc(bin, pos + 1, ["&gt;" | acc])
        ?" -> esc(bin, pos + 1, ["&quot;" | acc])
        ?' -> esc(bin, pos + 1, ["&#39;" | acc])
        c -> esc(bin, pos + 1, [c | acc])
      end
    end
  end

  defp encode_attrs(attrs) when is_map(attrs) do
    attrs
    |> :maps.to_list()
    |> Enum.map(fn {k, v} ->
      [" ", to_string(k), "=\"", escape(to_string(v)), "\""]
    end)
  end

  defp encode_attrs(_), do: []
end
