defmodule ElixirWorkers.Packer do
  @moduledoc false
  import Bitwise

  # AVM archive creator matching AtomVM's packbeam format.
  #
  # Processes standard .beam files: strips unnecessary chunks, decompresses
  # LitT -> LitU, and packages into a valid .avm archive.
  #
  # AVM Pack format (from avmpack.c / packbeam-format.md):
  #   - 24-byte header: "#!/usr/bin/env AtomVM\n\0\0"
  #   - Sections:
  #       - uint32 BE: total section size (header + name + data)
  #       - uint32 BE: flags (BEAM_START_FLAG=1, BEAM_CODE_FLAG=2)
  #       - uint32 BE: reserved (0)
  #       - name: null-terminated, padded to 4-byte boundary (includes .beam suffix)
  #       - processed BEAM data (IFF format, stripped chunks, decompressed literals)
  #   - End section: size=0, flags=0, reserved=0, name="end\0"

  @beam_start_flag 1
  @beam_code_flag 2

  @avm_header "#!/usr/bin/env AtomVM\n\0\0"

  # IFF chunks that AtomVM needs (LitT handled specially)
  @kept_chunks ["AtU8", "Code", "ExpT", "LocT", "ImpT",
                "LitU", "FunT", "StrT", "avmN", "Type"]

  @doc """
  Create an .avm archive from .beam files.

  - `output_path` - path to write the .avm file
  - `startup_beam` - filename of the startup module (e.g. "Elixir.MyApp.beam")
  - `beams_dir` - directory containing .beam files
  """
  def create_avm(output_path, startup_beam, beams_dir) do
    beam_files =
      beams_dir
      |> File.ls!()
      |> Enum.filter(&String.ends_with?(&1, ".beam"))
      |> Enum.sort()
      |> Enum.map(fn fname ->
        {fname, File.read!(Path.join(beams_dir, fname))}
      end)
      |> Map.new()

    unless Map.has_key?(beam_files, startup_beam) do
      available = beam_files |> Map.keys() |> Enum.sort() |> Enum.join(", ")
      raise "Startup module #{startup_beam} not found in #{beams_dir}. Available: #{available}"
    end

    sections = []

    # Startup module first (with START flag)
    startup_name = String.replace_trailing(startup_beam, ".beam", "")
    startup_data = Map.fetch!(beam_files, startup_beam)
    sections = [make_section(startup_name, startup_data, true) | sections]

    # Remaining modules
    sections =
      beam_files
      |> Enum.reject(fn {fname, _} -> fname == startup_beam end)
      |> Enum.sort_by(fn {fname, _} -> fname end)
      |> Enum.reduce(sections, fn {fname, data}, acc ->
        module_name = String.replace_trailing(fname, ".beam", "")
        [make_section(module_name, data, false) | acc]
      end)

    sections = Enum.reverse(sections)

    content = [@avm_header | sections] ++ [make_end_section()]
    File.write!(output_path, content)

    total_size = File.stat!(output_path).size
    {total_size, length(sections)}
  end

  @doc """
  Process a single BEAM binary for AtomVM: strip chunks, decompress LitT -> LitU.
  Returns the processed BEAM binary.
  """
  def process_beam(beam_data) do
    chunks = parse_iff_chunks(beam_data)
    out_chunks = process_chunks(chunks, [])

    # Reassemble IFF
    iff_body =
      Enum.map(out_chunks, fn {name, data} ->
        chunk = name <> <<byte_size(data)::32-big>> <> data
        pad_binary_to_4(chunk)
      end)

    # FOR1 header: total size = BEAM tag (4 bytes) + chunks
    iff_body_bin = IO.iodata_to_binary(iff_body)
    total_size = 4 + byte_size(iff_body_bin)
    <<"FOR1", total_size::32-big, "BEAM">> <> iff_body_bin
  end

  @doc """
  Parse a BEAM IFF file into a list of {name, data} tuples.
  """
  def parse_iff_chunks(<<"FOR1", _size::32-big, "BEAM", rest::binary>>) do
    parse_chunks(rest, [])
  end

  def parse_iff_chunks(_), do: raise("Not a valid BEAM file")

  @doc """
  Extract the set of module names imported by a BEAM binary.
  Returns a MapSet of strings like "Elixir.Enum", "erlang", etc.
  """
  def imported_modules(beam_data) do
    case :beam_lib.chunks(beam_data, [:imports]) do
      {:ok, {_mod, [{:imports, imports}]}} ->
        imports
        |> Enum.map(fn {m, _f, _a} -> Atom.to_string(m) end)
        |> MapSet.new()

      _ ->
        MapSet.new()
    end
  end

  # --- Internal ---

  defp parse_chunks(<<>>, acc), do: Enum.reverse(acc)

  defp parse_chunks(<<_::binary>> = bin, acc) when byte_size(bin) < 8 do
    Enum.reverse(acc)
  end

  defp parse_chunks(<<name::binary-size(4), size::32-big, rest::binary>>, acc) do
    data = binary_part(rest, 0, min(size, byte_size(rest)))
    padded_size = pad4(size + 8) - 8
    skip = min(padded_size, byte_size(rest))
    remaining = binary_part(rest, skip, byte_size(rest) - skip)
    parse_chunks(remaining, [{name, data} | acc])
  end

  defp process_chunks([], acc), do: Enum.reverse(acc)

  defp process_chunks([{"LitT", data} | rest], acc) do
    <<uncompressed_size::32-big, compressed_data::binary>> = data

    if uncompressed_size == 0 do
      # Already uncompressed -- keep as LitT
      process_chunks(rest, [{"LitT", data} | acc])
    else
      case safe_decompress(compressed_data) do
        {:ok, decompressed} ->
          process_chunks(rest, [{"LitU", decompressed} | acc])

        :error ->
          # Fall back to keeping LitT as-is
          process_chunks(rest, [{"LitT", data} | acc])
      end
    end
  end

  defp process_chunks([{name, data} | rest], acc) do
    if name in @kept_chunks do
      process_chunks(rest, [{name, data} | acc])
    else
      # Strip the chunk (Dbgi, CInf, Docs, Meta, etc.)
      process_chunks(rest, acc)
    end
  end

  defp safe_decompress(data) do
    try do
      {:ok, :zlib.uncompress(data)}
    rescue
      _ -> :error
    end
  end

  defp make_section(module_name, beam_data, is_start) do
    flags = if is_start, do: @beam_start_flag ||| @beam_code_flag, else: @beam_code_flag
    processed_beam = process_beam(beam_data)

    # Section name includes .beam suffix
    name_str = module_name <> ".beam"
    name_bytes = name_str <> <<0>>
    padded_name = pad_binary_to_4(name_bytes)

    # Pad beam data to 4-byte boundary
    padded_beam = pad_binary_to_4(processed_beam)

    # Total section size: 3 uint32s (12 bytes) + padded name + padded beam data
    total_size = 12 + byte_size(padded_name) + byte_size(padded_beam)

    <<total_size::32-big, flags::32-big, 0::32-big>> <> padded_name <> padded_beam
  end

  defp make_end_section do
    <<0::32-big, 0::32-big, 0::32-big, "end", 0>>
  end

  defp pad4(n) do
    case rem(n, 4) do
      0 -> n
      r -> n + (4 - r)
    end
  end

  defp pad_binary_to_4(bin) do
    case rem(byte_size(bin), 4) do
      0 -> bin
      r -> bin <> :binary.copy(<<0>>, 4 - r)
    end
  end
end
