# elixir-workers

Run Elixir on Cloudflare Workers via AtomVM compiled to WebAssembly (WASI).

## Architecture

Three layers:
1. **atomvm-wasi/** — C platform adapter implementing AtomVM's `sys.h` for WASI targets
2. **worker/** — Vanilla JS CF Worker that bridges HTTP ↔ stdin/stdout JSON protocol
3. **elixir-app/** — Elixir application code compiled to .beam, packaged as .avm

Request flow: `HTTP → JS shim → stdin (JSON) → AtomVM WASI → Elixir app → stdout (JSON) → HTTP Response`

## Build

```bash
make setup    # Clone AtomVM, install npm deps
make atomvm   # Compile AtomVM to WASM (requires ~/.wasi-sdk)
make app      # Compile Elixir + AtomVM stdlib → .avm archive
make dev      # Run local dev server on port 8797
make deploy   # Deploy to Cloudflare
```

### Build prerequisites

- **wasi-sdk** v24+ at `~/.wasi-sdk/` — C→WASM compiler ([github](https://github.com/WebAssembly/wasi-sdk))
- **AtomVM** — vendored in `vendor/AtomVM/` (cloned by `make setup`)
- **Elixir** 1.17+ / **Erlang/OTP** 26+ — for compiling .beam files
- **CMake** 3.20+ — for building the C code
- **Python 3** — for the AVM packer
- **wrangler** 4+ — for CF Workers dev/deploy (installed in worker/node_modules)
- Erlang must be on PATH: `export PATH="/opt/homebrew/opt/erlang/bin:$PATH"` (Makefile handles this)

### Build outputs

| Output | Size | Description |
|--------|------|-------------|
| `build/atomvm-wasi/atomvm.wasm` | ~559KB | AtomVM VM compiled to WASM (-Oz, LTO, wasm-opt) |
| `build/app.avm` | ~12KB | Packaged Elixir app + minimal stdlib (5 modules) |
| `worker/atomvm.wasm` | (copy) | Copied for wrangler |
| `worker/app.avm` | (copy) | Copied for wrangler |

## Key files

| File | Purpose |
|------|---------|
| `atomvm-wasi/src/sys.c` | WASI platform adapter — implements all `sys.h` functions |
| `atomvm-wasi/src/main.c` | WASI entry point — loads .avm from args, runs `start/0` |
| `atomvm-wasi/src/platform_nifs.c` | NIFs: `read_stdin/0`, `write_stdout/1`, `platform/0` |
| `atomvm-wasi/include/wasi_sys.h` | Platform data structures |
| `atomvm-wasi/include/wasi_compat.h` | Stubs for WASI-missing functions (tzset, etc.) |
| `atomvm-wasi/include/avm_version.h` | Generated version header for WASI build |
| `atomvm-wasi/CMakeLists.txt` | Build config — defines, excludes, wasi-sdk toolchain |
| `worker/src/index.js` | CF Worker: WASI runtime + HTTP↔JSON bridge (vanilla JS) |
| `worker/wrangler.jsonc` | Worker config with CompiledWasm/Data rules |
| `elixir-app/lib/elixir_workers.ex` | Entry point: `start/0` reads stdin, routes, writes stdout |
| `elixir-app/lib/elixir_workers/router.ex` | HTTP router — pattern matches method + path |
| `elixir-app/lib/elixir_workers/json.ex` | Minimal JSON encoder/decoder for AtomVM |
| `elixir-app/lib/atomvm/wasi.ex` | NIF bridge: `AtomVM.Wasi.read_stdin/0`, `write_stdout/1` |
| `scripts/build-atomvm.sh` | Compiles AtomVM C code to WASM via wasi-sdk + CMake |
| `scripts/build-app.sh` | Compiles Elixir/Erlang → .beam → .avm (3-step pipeline) |
| `scripts/pack_avm.py` | AVM archive creator matching AtomVM's packbeam format |

## Compilation constraints

AtomVM is compiled with these flags for WASI:
- `-Oz` — aggressive size optimization
- `-flto` — link-time optimization (dead code elimination across files)
- `-ffunction-sections -fdata-sections` + `-Wl,--gc-sections` — strip unused functions
- `AVM_NO_SMP` — single-threaded (CF Workers are single-threaded)
- `AVM_NO_JIT` — no JIT (WASM can't generate executable code)
- `ATOMVM_PLATFORM_WASI` — platform identifier
- `HAVE_OPEN_MEMSTREAM=0`, `AVM_DISABLE_NETWORKING=1`, `NDEBUG`
- Excluded C files: otp_socket, otp_net, inet, otp_ssl, otp_crypto, dist_nifs, jit, jit_stream_flash, portnifloader, posix_nifs, ets, ets_hashtable
- Unresolved env symbols (ETS, dist) are stubbed as no-ops in the JS WASI shim
- Post-build: `wasm-strip` (llvm-strip) + `wasm-opt -Oz` for further size reduction

## AVM pack format

The `.avm` file follows AtomVM's packbeam format:
- 24-byte header: `#!/usr/bin/env AtomVM\n\0\0`
- Sections: `[size:4 BE][flags:4 BE][reserved:4 BE][name.beam\0 padded][BEAM IFF data]`
- Flags: `BEAM_START_FLAG=1` (entry point), `BEAM_CODE_FLAG=2` (regular module), startup = `3` (both)
- End sentinel: `[0:4][0:4][0:4][end\0]`
- BEAM data is stripped (only AtU8, Code, ExpT, LocT, ImpT, LitT/LitU, FunT, StrT, avmN, Type kept)
- LitT with `uncompressed_size=0` (OTP 28+) is kept as-is; compressed LitT is decompressed to LitU

## NIF registration

| Elixir call | C NIF name | Implementation |
|-------------|-----------|----------------|
| `AtomVM.Wasi.read_stdin()` | `Elixir.AtomVM.Wasi:read_stdin/0` | Reads all of stdin into binary |
| `AtomVM.Wasi.write_stdout(data)` | `Elixir.AtomVM.Wasi:write_stdout/1` | Writes binary to stdout |
| `:atomvm.platform()` | `atomvm:platform/0` | Returns `:wasi` atom |

## JSON protocol

stdin (request):
```json
{"method":"GET","url":"/path","headers":{"host":"example.com"},"body":""}
```

stdout (response — key order not guaranteed):
```json
{"status":200,"headers":{"content-type":"text/html"},"body":"<h1>Hello</h1>"}
```

The JS deserializer finds the first complete JSON object in stdout using brace-counting (AtomVM may append `Return value: ok\n` after the JSON).

## Bundled stdlib modules

The .avm includes only 5 modules (aggressively pruned from 36):
- **app**: ElixirWorkers (entry), ElixirWorkers.Router, ElixirWorkers.JSON, AtomVM.Wasi
- **estdlib**: maps (for `maps:to_list/1`, `maps:put/3`, `maps:iterator/1`)

Most `erlang:*`, `lists:*`, `binary:*` functions are NIFs/BIFs built into the WASM binary — they don't need .beam files. The `init` module is intentionally excluded: when absent, AtomVM skips the OTP boot sequence and directly calls `start/0` on the startup module.

Add more from `vendor/AtomVM/libs/estdlib/src/` or `vendor/AtomVM/libs/exavmlib/lib/` as needed. Avoid `Map` (Elixir) — use `:maps.*` directly or pattern matching instead.

## Development rules

- The `worker/src/index.js` contains a full WASI implementation — do NOT replace it with a library. It's deliberately minimal and self-contained for CF Workers compatibility.
- The WASI shim includes `env` stubs for ETS/distribution functions — these are no-ops. Do not remove them.
- When modifying C code in `atomvm-wasi/`, always compile-test with `make atomvm` before committing.
- After modifying Elixir code, run `make app` to recompile and repack the .avm.
- The Elixir JSON module is intentionally minimal — it handles the HTTP protocol, not arbitrary JSON. Avoid string interpolation in Elixir code (pulls in String.Chars protocol). Use binary concatenation instead.
- Do not add `pthread.h` or any threading primitives to the WASI platform — it must remain single-threaded.
- The WASI build does NOT include zlib — all LitT chunks must be pre-decompressed by the packer.
- Port table: dev server runs on port **8797**.
- Run wrangler from the `worker/` directory: `cd worker && npx wrangler dev --port 8797`.
