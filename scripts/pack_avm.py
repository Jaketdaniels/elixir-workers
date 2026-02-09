#!/usr/bin/env python3
"""
AVM archive creator matching AtomVM's packbeam format.

Processes standard .beam files: strips unnecessary chunks, decompresses
LitT → LitU, and packages into a valid .avm archive.

AVM Pack format (from avmpack.c / packbeam-format.md):
  - 24-byte header: "#!/usr/bin/env AtomVM\n\0\0"
  - Sections:
      - uint32 BE: total section size (header + name + data)
      - uint32 BE: flags (BEAM_START_FLAG=1, BEAM_CODE_FLAG=2)
      - uint32 BE: reserved (0)
      - name: null-terminated, padded to 4-byte boundary (includes .beam suffix)
      - processed BEAM data (IFF format, stripped chunks, decompressed literals)
  - End section: size=0, flags=0, reserved=0, name="end\0"

Usage: pack_avm.py <output.avm> <startup_module.beam> <beams_directory>
"""

import os
import struct
import sys
import zlib

# From avmpack.h
BEAM_START_FLAG = 1
BEAM_CODE_FLAG = 2

AVM_HEADER = b"#!/usr/bin/env AtomVM\n\x00\x00"

# IFF chunks that AtomVM needs (from packbeam.c / iff.c)
# LitT is handled specially (decompress if needed), not listed here
KEPT_CHUNKS = {b"AtU8", b"Code", b"ExpT", b"LocT", b"ImpT",
               b"LitU", b"FunT", b"StrT", b"avmN", b"Type"}


def pad4(n):
    return (n + 3) & ~3


def read_beam_file(path):
    with open(path, "rb") as f:
        return f.read()


def parse_iff_chunks(beam_data):
    """Parse a BEAM IFF file into a list of (name, data) tuples."""
    if beam_data[:4] != b"FOR1" or beam_data[8:12] != b"BEAM":
        raise ValueError("Not a valid BEAM file")

    chunks = []
    pos = 12  # Skip FOR1 + size + BEAM
    while pos < len(beam_data):
        if pos + 8 > len(beam_data):
            break
        chunk_name = beam_data[pos:pos + 4]
        chunk_size = struct.unpack(">I", beam_data[pos + 4:pos + 8])[0]
        chunk_data = beam_data[pos + 8:pos + 8 + chunk_size]
        chunks.append((chunk_name, chunk_data))
        pos += pad4(chunk_size + 8)

    return chunks


def process_beam(beam_data):
    """Process a BEAM file for AtomVM: strip chunks, decompress LitT → LitU."""
    chunks = parse_iff_chunks(beam_data)
    out_chunks = []

    for name, data in chunks:
        if name == b"LitT":
            # LitT format: [uncompressed_size:4][data]
            # If uncompressed_size == 0, data is already uncompressed — keep as LitT
            # If uncompressed_size > 0, data is zlib-compressed — decompress to LitU
            uncompressed_size = struct.unpack(">I", data[:4])[0]
            if uncompressed_size == 0:
                # Already uncompressed — keep as LitT (AtomVM handles this)
                out_chunks.append((b"LitT", data))
            else:
                compressed_data = data[4:]
                try:
                    decompressed = zlib.decompress(compressed_data)
                except zlib.error as e:
                    print(f"Warning: Failed to decompress LitT: {e}", file=sys.stderr)
                    # Fall back to keeping the LitT as-is
                    out_chunks.append((b"LitT", data))
                    continue
                out_chunks.append((b"LitU", decompressed))
        elif name in KEPT_CHUNKS:
            out_chunks.append((name, data))
        # else: strip the chunk (Dbgi, CInf, Docs, Meta, etc.)

    # Reassemble IFF
    iff_body = b""
    for name, data in out_chunks:
        chunk = name + struct.pack(">I", len(data)) + data
        # Pad to 4-byte boundary
        while len(chunk) % 4 != 0:
            chunk += b"\x00"
        iff_body += chunk

    # FOR1 header: total size = BEAM tag (4 bytes) + chunks
    total_size = 4 + len(iff_body)
    return b"FOR1" + struct.pack(">I", total_size) + b"BEAM" + iff_body


def make_section(module_name, beam_data, is_start=False):
    """Create an AVM section for a processed .beam file."""
    flags = (BEAM_START_FLAG | BEAM_CODE_FLAG) if is_start else BEAM_CODE_FLAG

    # Process the BEAM file (strip chunks, decompress literals)
    processed_beam = process_beam(beam_data)

    # Section name includes .beam suffix
    name_str = module_name + ".beam"
    name_bytes = name_str.encode("utf-8") + b"\x00"
    padded_name_len = pad4(len(name_bytes))
    name_bytes = name_bytes.ljust(padded_name_len, b"\x00")

    # Pad beam data to 4-byte boundary
    padded_beam = processed_beam
    while len(padded_beam) % 4 != 0:
        padded_beam += b"\x00"

    # Total section size: 3 uint32s (12 bytes) + padded name + padded beam data
    total_size = 12 + padded_name_len + len(padded_beam)

    section = struct.pack(">III", total_size, flags, 0)
    section += name_bytes
    section += padded_beam
    return section


def make_end_section():
    """Create the end sentinel section."""
    name = b"end\x00"
    return struct.pack(">III", 0, 0, 0) + name


def create_avm(output_path, startup_beam, beams_dir):
    """Create an .avm archive from .beam files."""
    sections = []

    beam_files = {}
    for fname in sorted(os.listdir(beams_dir)):
        if fname.endswith(".beam"):
            fpath = os.path.join(beams_dir, fname)
            beam_files[fname] = read_beam_file(fpath)

    if startup_beam not in beam_files:
        print(f"Error: startup module {startup_beam} not found in {beams_dir}")
        print(f"Available: {', '.join(sorted(beam_files.keys()))}")
        sys.exit(1)

    # Startup module first (with START flag)
    startup_name = startup_beam.replace(".beam", "")
    print(f"  [start] {startup_name}")
    sections.append(make_section(startup_name, beam_files[startup_beam], is_start=True))

    # Remaining modules
    for fname, data in beam_files.items():
        if fname != startup_beam:
            module_name = fname.replace(".beam", "")
            print(f"  [code]  {module_name}")
            sections.append(make_section(module_name, data, is_start=False))

    with open(output_path, "wb") as f:
        f.write(AVM_HEADER)
        for section in sections:
            f.write(section)
        f.write(make_end_section())

    total_size = os.path.getsize(output_path)
    print(f"Created {output_path} ({total_size} bytes, {len(sections)} modules)")


if __name__ == "__main__":
    if len(sys.argv) != 4:
        print(f"Usage: {sys.argv[0]} <output.avm> <startup.beam> <beams_dir>")
        sys.exit(1)

    create_avm(sys.argv[1], sys.argv[2], sys.argv[3])
