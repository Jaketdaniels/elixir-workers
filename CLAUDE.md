# elixir-workers

Run Elixir on Cloudflare Workers via AtomVM compiled to WebAssembly (WASI).

## Project structure

Two-package monorepo following the Phoenix model:

| Package | Location | Purpose |
|---------|----------|---------|
| `elixir_workers` | `packages/elixir_workers/` | Hex package: framework modules + Mix tasks + pre-built artifacts |
| `elixir_workers_new` | `packages/elixir_workers_new/` | Hex archive: project generator (`mix elixir_workers.new`) |

```
elixir-workers/
├── packages/
│   ├── elixir_workers/           # Framework package
│   │   ├── lib/
│   │   │   ├── elixir_workers.ex           # Public API (read_request, write_response)
│   │   │   ├── elixir_workers/
│   │   │   │   ├── app.ex                  # `use ElixirWorkers.App` macro
│   │   │   │   ├── router.ex              # `use ElixirWorkers.Router` macro
│   │   │   │   ├── conn.ex                # Connection map + response builders
│   │   │   │   ├── json.ex                # JSON encoder/decoder
│   │   │   │   ├── url.ex                 # URL parsing, path matching
│   │   │   │   ├── body.ex                # Request body parsing
│   │   │   │   ├── middleware.ex           # Composable middleware
│   │   │   │   ├── kv.ex                  # CF KV access (two-pass)
│   │   │   │   ├── d1.ex                  # CF D1 access (two-pass)
│   │   │   │   ├── html.ex                # HTML escaping, tag helpers
│   │   │   │   ├── wasi.ex                # NIF bridge (read_stdin/write_stdout)
│   │   │   │   └── packer.ex              # AVM packer (ported from Python)
│   │   │   └── mix/tasks/
│   │   │       ├── elixir_workers.build.ex # Compile + pack .avm
│   │   │       ├── elixir_workers.dev.ex   # Build + wrangler dev
│   │   │       └── elixir_workers.deploy.ex# Build + wrangler deploy
│   │   └── priv/
│   │       ├── atomvm.wasm                 # Pre-built VM (559 KB)
│   │       ├── stdlib/                     # Pre-compiled .beam files
│   │       └── templates/                  # JS worker, wrangler config
│   └── elixir_workers_new/       # Generator archive
│       ├── lib/mix/tasks/
│       │   └── elixir_workers.new.ex
│       └── priv/templates/               # Project scaffold templates
├── atomvm-wasi/                  # C platform adapter (dev/CI only)
├── vendor/AtomVM/                # VM source (for rebuilding WASM)
├── elixir-app/                   # Legacy app (being replaced by packages/)
├── scripts/                      # Build scripts
├── examples/hello_world/         # Example project (dogfood)
└── worker/                       # Legacy worker dir
```

## User workflow (new projects)

```bash
mix elixir_workers.new my_app
cd my_app && mix deps.get
mix elixir_workers.dev          # builds .avm + starts wrangler on :8797
```

Users only need: Elixir/Erlang + Node.js. No wasi-sdk, cmake, binaryen, or python.

## Architecture

Three layers:
1. **worker/src/index.js** — Vanilla JS Cloudflare Worker. Implements WASI. Bridges HTTP↔JSON.
2. **atomvm-wasi/** — C platform adapter. NIFs for stdin/stdout. Compiled to WASM via wasi-sdk.
3. **User Elixir code** — compiled to .beam, packed into .avm with stdlib.

Request flow: `HTTP → JS Worker → stdin (JSON) → AtomVM WASM → Elixir → stdout (JSON) → HTTP Response`

Each request creates a fresh WASM instance. No state between requests.

### Two-pass binding protocol

For CF bindings (KV, D1):
1. **Pass 1**: WASM runs → returns `{"_needs": [...]}` listing binding data it needs
2. **Pass 2**: JS fulfills needs in parallel → re-runs WASM with fulfilled data
3. **Post-response**: JS executes write effects via `ctx.waitUntil()`

Routes without bindings have zero overhead (single pass).

### App and Router macros

`use ElixirWorkers.App, router: MyRouter` generates a `start/0` that:
- Reads JSON from stdin via NIF
- Creates a conn map
- Routes through the specified router
- Writes JSON response to stdout

`use ElixirWorkers.Router` injects:
- `call/1` — middleware pipeline + dispatch
- `dispatch/1` — match method + path segments from `routes/0`
- `find_route/3` — pattern matching with `:param` captures
- `not_found/1` — default 404 handler
- Default `middleware/0` — security headers + body parsing (overridable)

User implements `routes/0` returning `[{method, pattern, handler}]`.

## Build (framework development)

```bash
make setup    # Clone AtomVM, install npm deps
make          # Build everything: WASM + .avm
make app      # Rebuild just the Elixir app
make priv     # Pre-compile stdlib into packages/elixir_workers/priv/
make dev      # Dev server on :8797
```

### Build prerequisites (framework development only)

- **wasi-sdk** v24+ at `~/.wasi-sdk/`
- **CMake** 3.20+
- **Python 3** — legacy AVM packer (Elixir port in packer.ex)
- **Binaryen** — `wasm-opt`
- **Elixir** 1.17+ / **Erlang/OTP** 26+
- **wrangler** 4+

### Key pre-built artifacts

| Artifact | Size | Description |
|----------|------|-------------|
| `priv/atomvm.wasm` | 559 KB | AtomVM VM compiled to WASM |
| `priv/stdlib/*.beam` | ~91 files | AtomVM Elixir+Erlang stdlib |

These are static — only change when the VM is updated, not when user code changes.

## Key files

| File | Purpose |
|------|---------|
| `packages/elixir_workers/lib/elixir_workers.ex` | Public API: read_request/0, write_response/1 |
| `packages/elixir_workers/lib/elixir_workers/app.ex` | `use ElixirWorkers.App` macro (generates start/0) |
| `packages/elixir_workers/lib/elixir_workers/router.ex` | `use ElixirWorkers.Router` macro (dispatch framework) |
| `packages/elixir_workers/lib/elixir_workers/packer.ex` | AVM archive packer (Elixir port of pack_avm.py) |
| `packages/elixir_workers/lib/mix/tasks/*.ex` | Mix tasks: build, dev, deploy |
| `packages/elixir_workers/priv/templates/index.js` | JS worker template (WASI runtime) |
| `packages/elixir_workers_new/lib/mix/tasks/elixir_workers.new.ex` | Project generator |
| `worker/src/index.js` | CF Worker: full WASI runtime + HTTP↔JSON bridge |
| `worker/wrangler.jsonc` | Worker config (port 8797) |
| `atomvm-wasi/src/main.c` | WASI entry point — loads .avm, calls start/0 |
| `atomvm-wasi/src/platform_nifs.c` | NIFs: read_stdin/0, write_stdout/1 |
| `scripts/pack_avm.py` | Legacy Python AVM packer |

## NIF registration

| Elixir call | C function | What it does |
|-------------|-----------|--------------|
| `ElixirWorkers.Wasi.read_stdin()` | `nif_wasi_read_stdin` | Reads all stdin into a binary |
| `ElixirWorkers.Wasi.write_stdout(data)` | `nif_wasi_write_stdout` | Writes binary to stdout |
| `:atomvm.platform()` | `nif_atomvm_platform` | Returns `:wasi` atom |

Note: The NIF is registered under `AtomVM.Wasi` in the C code. The Elixir module `ElixirWorkers.Wasi` must compile to a BEAM module named `Elixir.ElixirWorkers.Wasi` — but the NIF lookup is by atom. The legacy `AtomVM.Wasi` module name is still used for NIF registration in the C code; the `ElixirWorkers.Wasi` module delegates to the same NIF stubs.

## AVM pack format

The `.avm` file follows AtomVM's packbeam format:
- 24-byte header: `#!/usr/bin/env AtomVM\n\0\0`
- Sections: `[size:4 BE][flags:4 BE][reserved:4 BE][name.beam\0 padded][BEAM IFF data]`
- Flags: `BEAM_START_FLAG=1` (entry point), `BEAM_CODE_FLAG=2` (regular module)
- End sentinel: `[0:4][0:4][0:4][end\0]`
- BEAM data stripped (keeps AtU8, Code, ExpT, LocT, ImpT, LitT/LitU, FunT, StrT, avmN, Type)
- Compressed LitT decompressed to LitU (WASI has no zlib)

The Elixir packer (`ElixirWorkers.Packer`) reproduces the exact same binary format as `scripts/pack_avm.py`.

## Development rules

- `worker/src/index.js` is a complete WASI implementation — don't replace with a library
- The WASI shim includes `env` stubs for ETS/distribution — no-ops, don't remove
- The JSON module uses iolist assembly, not `Enum.intersperse` (unavailable in AtomVM)
- No threading — `AVM_NO_SMP`, single-threaded only
- No zlib in WASI build — LitT chunks must be pre-decompressed by packer
- Port: dev server runs on **8797**
- The NIF module name in C is `AtomVM.Wasi` — keep `ElixirWorkers.Wasi` as the Elixir wrapper
- Framework modules live in `packages/elixir_workers/lib/`; legacy `elixir-app/` is for reference
