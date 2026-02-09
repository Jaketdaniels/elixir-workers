# elixir-workers

Run Elixir on Cloudflare Workers.

AtomVM (a tiny BEAM VM written in C) is compiled to WebAssembly, letting your Elixir code run inside Cloudflare's edge network. Pattern matching, immutable data, and all the good parts of Elixir — on every continent, cold starts under 35ms.

```
HTTP request → JS Worker → stdin (JSON) → AtomVM (WASM) → Elixir app → stdout (JSON) → HTTP response
```

## Setup

### 1. Install prerequisites

**macOS (Homebrew):**

```bash
brew install cmake elixir node
```

You also need [wasi-sdk](https://github.com/WebAssembly/wasi-sdk/releases) — the C-to-WebAssembly compiler:

```bash
# Download the latest release for your platform, then:
export WASI_SDK_VERSION=25  # or whatever's latest
curl -LO https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${WASI_SDK_VERSION}/wasi-sdk-${WASI_SDK_VERSION}.0-arm64-macos.tar.gz
tar xf wasi-sdk-*.tar.gz
mv wasi-sdk-${WASI_SDK_VERSION}.0-arm64-macos ~/.wasi-sdk
```

For x86 Mac or Linux, grab the matching archive from the releases page.

Optional (recommended) — install [binaryen](https://github.com/WebAssembly/binaryen) for smaller WASM output:

```bash
brew install binaryen  # provides wasm-opt
```

### 2. Clone and build

```bash
git clone https://github.com/Jaketdaniels/elixir-workers.git
cd elixir-workers
make setup   # clones AtomVM source, installs npm deps
make         # compiles AtomVM → WASM, compiles Elixir → .avm
```

### 3. Run locally

```bash
make dev     # starts wrangler dev server on localhost:8797
```

Try it:

```bash
curl http://localhost:8797/
curl http://localhost:8797/api/health
curl -X POST -d '{"hello":"world"}' http://localhost:8797/api/echo
```

### 4. Deploy

```bash
make deploy  # deploys to Cloudflare (requires wrangler login)
```

## Writing your app

All your Elixir code lives in `elixir-app/lib/`. The entry point is `ElixirWorkers.start/0`. After editing, run `make app` to rebuild.

### Adding a route

Open `elixir-app/lib/elixir_workers/router.ex` and add a function clause:

```elixir
def handle(%{"method" => "GET", "url" => "/api/hello"} = _req) do
  %{
    "status" => 200,
    "headers" => %{"content-type" => "application/json"},
    "body" => ElixirWorkers.JSON.encode(%{"hello" => "world"})
  }
end
```

Routes are matched top-to-bottom. The last clause returns a 404.

### Request shape

Your handler receives a map with string keys:

```elixir
%{
  "method"  => "POST",
  "url"     => "/api/users?page=1",
  "headers" => %{"content-type" => "application/json"},
  "body"    => "{\"name\":\"Alice\"}"
}
```

### Response shape

Return a map with `"status"`, `"headers"`, and `"body"`:

```elixir
%{
  "status"  => 201,
  "headers" => %{"content-type" => "application/json"},
  "body"    => ElixirWorkers.JSON.encode(%{"created" => true})
}
```

## What's included

The full AtomVM standard library is bundled (84 modules). Write normal Elixir — string interpolation, `Enum`, `Map`, `Keyword`, `GenServer`, `Supervisor`, `spawn`, `Process`, protocols, everything you'd expect.

**Elixir stdlib:** Enum, Map, List, Keyword, MapSet, String.Chars, Enumerable, Collectable, Range, Integer, Tuple, IO, Base, Bitwise, Process, Module, Function, System, Access, Protocol, exceptions

**OTP behaviours:** GenServer, GenStatem, GenEvent, Supervisor, proc_lib, sys, timer

**Erlang stdlib:** maps, lists, proplists, queue, sets, io, io_lib, string, unicode, math, base64, calendar, gen, gen_server, gen_statem, gen_event, supervisor

**Platform limits** (Cloudflare Workers, not Elixir):
- No direct network sockets (all I/O goes through the HTTP request/response bridge)
- No persistent file system (use Cloudflare KV/R2 for storage)

## Project layout

```
elixir-workers/
├── elixir-app/lib/          ← your Elixir code goes here
│   ├── elixir_workers.ex        entry point (start/0)
│   ├── elixir_workers/
│   │   ├── router.ex            HTTP routing
│   │   └── json.ex              JSON encoder/decoder
│   └── atomvm/wasi.ex           stdin/stdout NIFs
├── worker/                  ← Cloudflare Worker (JS glue)
│   ├── src/index.js             WASI runtime + HTTP bridge
│   └── wrangler.jsonc           worker config
├── atomvm-wasi/             ← C platform layer (AtomVM → WASI)
│   ├── src/                     sys.c, main.c, platform_nifs.c
│   ├── include/                 headers
│   └── CMakeLists.txt           build config
├── scripts/                 ← build pipeline
│   ├── build-atomvm.sh          C → WASM compilation
│   ├── build-app.sh             Elixir → .beam → .avm packaging
│   └── pack_avm.py              AVM archive creator
├── vendor/AtomVM/           ← AtomVM source (cloned by make setup)
└── Makefile
```

## Bundle size

| Component | Raw | Gzipped |
|-----------|-----|---------|
| AtomVM WASM | 559 KB | — |
| Elixir app (.avm) | 227 KB | — |
| JS worker | 7 KB | — |
| **Total bundle** | **817 KB** | **240 KB** |

Cloudflare measures the gzipped size against the limit (3 MB free / 10 MB paid). At 240 KB you're using 8% of the free tier.

## License

Apache-2.0

AtomVM is licensed under Apache-2.0 OR LGPL-2.1-or-later.
