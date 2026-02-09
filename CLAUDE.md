# elixir-workers

Run Elixir on Cloudflare Workers via AtomVM compiled to WebAssembly (WASI).

## Architecture

Three layers:
1. **worker/src/index.js** — Vanilla JS Cloudflare Worker. Implements WASI (stdin/stdout/clock/random). Bridges HTTP requests to JSON on stdin, reads JSON response from stdout.
2. **atomvm-wasi/** — C platform adapter. Implements AtomVM's `sys.h` for WASI. Provides NIFs for stdin/stdout. Compiled to WASM via wasi-sdk.
3. **elixir-app/lib/** — Your Elixir code. Compiled to .beam bytecode, packaged into an .avm archive with the full AtomVM stdlib (84 modules).

Request flow: `HTTP → JS Worker → stdin (JSON) → AtomVM WASM → Elixir app → stdout (JSON) → HTTP Response`

Each request creates a fresh WASM instance. No state carries over between requests.

### Two-pass binding protocol

Elixir runs synchronously in WASM and can't call async JS APIs directly. To access CF bindings (KV, D1), the framework uses a two-pass protocol:

1. **Pass 1**: JS sends enriched request (with `env`, `cf` properties) → WASM runs → Elixir returns either a final HTTP response OR a `{"_needs": [...]}` response listing binding data it needs.
2. **Pass 2** (only if `_needs` returned): JS fulfills all needs in parallel (KV.get, D1.query) → re-runs WASM with fulfilled data in `"bindings"` field → Elixir returns final response + optional `"_effects"`.
3. **Post-response**: JS executes write effects (KV.put, D1.exec) via `ctx.waitUntil()`.

Routes that don't use bindings have zero overhead (single pass, same as before).

## Build

```bash
make setup    # Clone AtomVM source, install npm deps (first time only)
make          # Build everything: AtomVM → WASM, Elixir → .avm
make app      # Rebuild just the Elixir app (fast, use while developing)
make dev      # Local dev server on port 8797
make deploy   # Deploy to Cloudflare
```

### Build prerequisites

- **wasi-sdk** v24+ at `~/.wasi-sdk/` — C→WASM compiler
- **AtomVM** — vendored in `vendor/AtomVM/` (cloned by `make setup`)
- **Elixir** 1.17+ / **Erlang/OTP** 26+ — compiles .ex/.erl → .beam
- **CMake** 3.20+ — builds the C code
- **Python 3** — runs the AVM packer
- **Binaryen** — `wasm-opt` for WASM size optimization
- **wrangler** 4+ — CF Workers dev/deploy (installed in worker/node_modules)

### Build outputs

| Output | Size | Description |
|--------|------|-------------|
| `worker/atomvm.wasm` | 559 KB | AtomVM VM compiled to WASM (-Oz, LTO, wasm-opt) |
| `worker/app.avm` | 245 KB | Elixir app + full stdlib (91 modules) |
| **Total gzipped** | **240 KB** | 8% of CF free tier (3 MB limit) |

### Build pipeline

**`make atomvm`** → `scripts/build-atomvm.sh`:
1. CMake configure with wasi-sdk toolchain
2. Compile AtomVM C source with `-Oz -flto -ffunction-sections -fdata-sections`
3. Link with `--gc-sections --strip-debug`
4. Post-process: `wasm-strip` + `wasm-opt -Oz`
5. Copy to `worker/atomvm.wasm`

**`make app`** → `scripts/build-app.sh`:
1. Compile Erlang estdlib modules (gen_server, maps, lists, etc.) with `erlc`
2. Compile Elixir exavmlib modules (Enum, Map, GenServer, etc.) with `elixirc` — excludes hardware modules (GPIO, I2C, LEDC, AVMPort, Console)
3. Auto-discover and compile all `.ex` files in `elixir-app/lib/` with `elixirc`
4. Auto-detect startup module (first file containing `def start`)
5. Pack all .beam files into .avm archive with `pack_avm.py`
6. Copy to `worker/app.avm`

## Key files

| File | Purpose |
|------|---------|
| `worker/src/index.js` | CF Worker: full WASI runtime + HTTP↔JSON bridge |
| `worker/wrangler.jsonc` | Worker config (port 8797, CompiledWasm/Data rules) |
| `atomvm-wasi/src/main.c` | WASI entry point — loads .avm, calls `start/0` |
| `atomvm-wasi/src/sys.c` | Platform adapter — time, polling stubs, file I/O |
| `atomvm-wasi/src/platform_nifs.c` | NIFs: read_stdin/0, write_stdout/1, platform/0 |
| `atomvm-wasi/CMakeLists.txt` | Build config — flags, excluded modules, toolchain |
| `elixir-app/lib/elixir_workers.ex` | Entry point: start/0 reads stdin, routes, writes stdout |
| `elixir-app/lib/elixir_workers/conn.ex` | Connection map builder + response helpers (html, json, text) |
| `elixir-app/lib/elixir_workers/router.ex` | HTTP router — route table, path params, middleware pipeline |
| `elixir-app/lib/elixir_workers/json.ex` | JSON encoder/decoder (no deps, uses iolist) |
| `elixir-app/lib/elixir_workers/url.ex` | URL parsing, query decoding, path matching |
| `elixir-app/lib/elixir_workers/body.ex` | Request body parsing (urlencoded, JSON) |
| `elixir-app/lib/elixir_workers/html.ex` | HTML escaping, tag building helpers |
| `elixir-app/lib/elixir_workers/kv.ex` | CF KV access (transparent two-pass reads, write effects) |
| `elixir-app/lib/elixir_workers/d1.ex` | CF D1 SQLite access (transparent two-pass reads, write effects) |
| `elixir-app/lib/elixir_workers/middleware.ex` | Composable middleware (security headers, CORS, body parsing) |
| `elixir-app/lib/atomvm/wasi.ex` | NIF bridge stubs for read_stdin/write_stdout |
| `scripts/build-atomvm.sh` | Compiles AtomVM C → WASM via wasi-sdk + CMake |
| `scripts/build-app.sh` | Compiles Elixir/Erlang → .beam → .avm |
| `scripts/pack_avm.py` | AVM archive creator (strips debug chunks, handles LitT) |

## Bundled stdlib (91 modules)

The full AtomVM standard library is bundled. All standard Elixir modules work:

**Elixir:** Enum, Map, List, Keyword, MapSet, String.Chars, Enumerable, Collectable, Range, Integer, Tuple, IO, Base, Bitwise, Process, Module, Function, System, Access, Protocol

**OTP:** GenServer, GenStatem, GenEvent, Supervisor, proc_lib, sys, timer

**Erlang:** gen, gen_server, gen_statem, gen_event, supervisor, maps, lists, proplists, queue, sets, io, io_lib, string, unicode, timer, math, base64, calendar

Additionally, most `erlang:*`, `lists:*`, `binary:*` functions are BIFs built into the WASM binary — they don't need .beam files.

**Excluded** (hardware-specific, no WASI support): GPIO, I2C, LEDC, AVMPort, Console

The `init` module is intentionally excluded. When absent, AtomVM skips the OTP boot sequence and directly calls `start/0` on the startup module.

## Compilation flags

AtomVM is compiled with:
- `-Oz` — aggressive size optimization
- `-flto` — link-time optimization
- `-ffunction-sections -fdata-sections` + `-Wl,--gc-sections` — strip unused functions
- `AVM_NO_SMP` — single-threaded (CF Workers are single-threaded)
- `AVM_NO_JIT` — no JIT (WASM can't generate executable code)
- `ATOMVM_PLATFORM_WASI`, `AVM_DISABLE_NETWORKING=1`, `NDEBUG`
- Excluded C: otp_socket, otp_net, inet, otp_ssl, otp_crypto, dist_nifs, jit, posix_nifs, ets, ets_hashtable

Unresolved `env` symbols (ETS, distribution) are stubbed as no-ops in the JS WASI shim.

## NIF registration

| Elixir call | C function | What it does |
|-------------|-----------|--------------|
| `AtomVM.Wasi.read_stdin()` | `nif_wasi_read_stdin` | Reads all stdin into a binary |
| `AtomVM.Wasi.write_stdout(data)` | `nif_wasi_write_stdout` | Writes binary to stdout |
| `:atomvm.platform()` | `nif_atomvm_platform` | Returns `:wasi` atom |

## JSON protocol

stdin (request — enriched by JS):
```json
{"method":"GET","url":"/path?q=1","headers":{"host":"example.com"},"body":"","env":{"APP_NAME":"my-app"},"cf":{"colo":"DFW"}}
```

stdout (final response):
```json
{"status":200,"headers":{"content-type":"text/html"},"body":"<h1>Hello</h1>"}
```

stdout (needs bindings — triggers pass 2):
```json
{"_needs":[{"type":"kv_get","ns":"MY_KV","key":"user:1","id":"kv_get:MY_KV:user:1"}],"_state":{}}
```

stdout (response with effects — writes executed post-response):
```json
{"status":200,"headers":{},"body":"ok","_effects":[{"type":"kv_put","ns":"MY_KV","key":"x","value":"y"}]}
```

The JS shim extracts the first complete JSON object from stdout using brace-counting (AtomVM appends `Return value: ok\n` after the JSON).

## AVM pack format

The `.avm` file follows AtomVM's packbeam format:
- 24-byte header: `#!/usr/bin/env AtomVM\n\0\0`
- Sections: `[size:4 BE][flags:4 BE][reserved:4 BE][name.beam\0 padded][BEAM IFF data]`
- Flags: `BEAM_START_FLAG=1` (entry point), `BEAM_CODE_FLAG=2` (regular module)
- End sentinel: `[0:4][0:4][0:4][end\0]`
- BEAM data is stripped (keeps AtU8, Code, ExpT, LocT, ImpT, LitT/LitU, FunT, StrT, avmN, Type)
- Compressed LitT chunks are decompressed to LitU (WASI build has no zlib)

## Development rules

- `worker/src/index.js` is a complete WASI implementation — don't replace with a library
- The WASI shim includes `env` stubs for ETS/distribution — no-ops, don't remove
- After modifying C code: `make atomvm` to rebuild WASM
- After modifying Elixir code: `make app` to rebuild .avm
- The JSON module uses iolist assembly + custom `join/3` instead of `Enum.intersperse` (not available in AtomVM's Enum)
- No threading — `AVM_NO_SMP`, single-threaded only
- No zlib in WASI build — LitT chunks must be pre-decompressed by packer
- Port: dev server runs on **8797**
- Run wrangler from `worker/`: `cd worker && npx wrangler dev`
