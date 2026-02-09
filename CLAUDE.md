# elixir-workers

Run Elixir on Cloudflare Workers via AtomVM compiled to WebAssembly (WASI).

## Project structure

Single-package architecture:

```
elixir-workers/
├── packages/
│   └── elixir_workers/             # Hex package
│       ├── lib/
│       │   ├── elixir_workers.ex           # Public API (read_request, write_response)
│       │   ├── elixir_workers/
│       │   │   ├── app.ex                  # `use ElixirWorkers.App` macro
│       │   │   ├── router.ex              # `use ElixirWorkers.Router` macro
│       │   │   ├── conn.ex                # Connection map + response builders
│       │   │   ├── json.ex                # JSON encoder/decoder
│       │   │   ├── url.ex                 # URL parsing, path matching
│       │   │   ├── body.ex                # Request body parsing
│       │   │   ├── middleware.ex           # Composable middleware
│       │   │   ├── kv.ex                  # CF KV access (two-pass)
│       │   │   ├── d1.ex                  # CF D1 access (two-pass)
│       │   │   ├── html.ex                # HTML escaping, tag helpers
│       │   │   ├── atomvm_wasi.ex         # NIF stubs (AtomVM.Wasi module)
│       │   │   ├── wasi.ex                # NIF bridge (delegates to AtomVM.Wasi)
│       │   │   └── packer.ex              # AVM packer (ported from Python)
│       │   └── mix/tasks/
│       │       ├── elixir_workers.new.ex  # Project generator
│       │       ├── elixir_workers.build.ex # Compile + pack .avm
│       │       ├── elixir_workers.dev.ex   # Build + wrangler dev
│       │       └── elixir_workers.deploy.ex# Build + wrangler deploy
│       └── priv/
│           ├── atomvm.wasm                 # Pre-built VM (559 KB)
│           ├── stdlib/                     # Pre-compiled .beam files
│           └── templates/                  # JS worker, wrangler config, project scaffold
├── atomvm-wasi/                  # C platform adapter (dev/CI only)
├── vendor/AtomVM/                # VM source (for rebuilding WASM)
├── _build/starter/               # Generated from templates (make dev)
├── scripts/                      # Build scripts
└── worker/                       # Legacy dev worker dir
```

## User workflow

```bash
mix archive.install hex elixir_workers
mix elixir_workers.new my_app
cd my_app && mix deps.get
mix elixir_workers.dev          # builds .avm + starts wrangler on :8797
mix elixir_workers.deploy       # builds + deploys to CF Workers
```

Users only need: Elixir/Erlang + Node.js. No wasi-sdk, cmake, binaryen, or python.

### Generated project structure

```
my_app/
├── lib/
│   ├── my_app.ex           # use ElixirWorkers.App
│   └── my_app/router.ex    # routes & handlers
├── mix.exs
├── wrangler.jsonc           # CF Workers config (user edits for bindings)
├── package.json             # wrangler npm dep
├── node_modules/            # gitignored
├── _build/                  # gitignored — contains worker/src/index.js, atomvm.wasm, app.avm
├── deps/                    # gitignored
└── .gitignore
```

`wrangler.jsonc` and `package.json` live at the project root for user access (bindings, env vars).
Build artifacts (`index.js`, `atomvm.wasm`, `app.avm`) go in `_build/worker/` — framework internals.

## Architecture

Three layers:
1. **priv/templates/index.js** — Vanilla JS Cloudflare Worker. Implements WASI. Bridges HTTP↔JSON. Copied to `_build/worker/` on build.
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
make app      # Rebuild just the Elixir app (legacy shell-script path)
make priv     # Pre-compile stdlib into packages/elixir_workers/priv/
make dev      # Generate starter from templates + dev server on :8797
make clean    # Remove build/ and _build/starter/
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
| `priv/stdlib/*.beam` | ~111 files | AtomVM Elixir+Erlang stdlib |

These are static — only change when the VM is updated, not when user code changes.

## Key files

| File | Purpose |
|------|---------|
| `packages/elixir_workers/lib/elixir_workers.ex` | Public API: read_request/0, write_response/1 |
| `packages/elixir_workers/lib/elixir_workers/app.ex` | `use ElixirWorkers.App` macro (generates start/0) |
| `packages/elixir_workers/lib/elixir_workers/router.ex` | `use ElixirWorkers.Router` macro (dispatch framework) |
| `packages/elixir_workers/lib/elixir_workers/packer.ex` | AVM archive packer (Elixir port of pack_avm.py) |
| `packages/elixir_workers/lib/elixir_workers/atomvm_wasi.ex` | `AtomVM.Wasi` module — NIF stubs matching C registration |
| `packages/elixir_workers/lib/elixir_workers/wasi.ex` | `ElixirWorkers.Wasi` — delegates to AtomVM.Wasi |
| `packages/elixir_workers/lib/mix/tasks/*.ex` | Mix tasks: new, build, dev, deploy |
| `packages/elixir_workers/priv/templates/index.js` | JS worker template (WASI runtime) |
| `packages/elixir_workers/priv/templates/*.eex` | Project scaffold templates |
| `worker/src/index.js` | Legacy dev worker (framework dev only) |
| `atomvm-wasi/src/main.c` | WASI entry point — loads .avm, calls start/0 |
| `atomvm-wasi/src/platform_nifs.c` | NIFs: read_stdin/0, write_stdout/1 |
| `scripts/pack_avm.py` | Legacy Python AVM packer |

## NIF registration

The C code in `atomvm-wasi/src/platform_nifs.c` registers NIFs under the module name `AtomVM.Wasi`:

| C NIF name | Elixir module | Function |
|------------|---------------|----------|
| `Elixir.AtomVM.Wasi:read_stdin/0` | `AtomVM.Wasi` | Reads all stdin into a binary |
| `Elixir.AtomVM.Wasi:write_stdout/1` | `AtomVM.Wasi` | Writes binary to stdout |
| `atomvm:platform/0` | `:atomvm` | Returns `:wasi` atom |

`AtomVM.Wasi` (in `atomvm_wasi.ex`) contains the NIF stubs. `ElixirWorkers.Wasi` delegates to it for use by the framework.

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

- `priv/templates/index.js` is a complete WASI implementation — don't replace with a library
- The WASI shim includes `env` stubs for ETS/distribution — no-ops, don't remove
- The JSON module uses iolist assembly, not `Enum.intersperse` (unavailable in AtomVM)
- No threading — `AVM_NO_SMP`, single-threaded only
- No zlib in WASI build — LitT chunks must be pre-decompressed by packer
- Port: dev server runs on **8797**
- The NIF module name in C is `AtomVM.Wasi` — the `AtomVM.Wasi` Elixir module must exist for NIF resolution
- Framework modules live in `packages/elixir_workers/lib/`
