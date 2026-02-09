# elixir-workers

Write Elixir, deploy to Cloudflare Workers. Cold starts under 35ms.

A tiny BEAM VM ([AtomVM](https://github.com/atomvm/AtomVM)) compiled to WebAssembly lets you use pattern matching, immutable data, GenServer, Supervisor, and everything else you like about Elixir — running on Cloudflare's edge network in 300+ cities.

## Why

- **Just write Elixir.** 84 standard library modules are bundled. `Enum`, `Map`, `GenServer`, `Supervisor`, protocols — it all works out of the box.
- **Fast.** Sub-35ms cold starts. Your code runs close to your users.
- **Small.** 240 KB gzipped total — 8% of Cloudflare's free tier limit.
- **Simple.** One `make` command to build. One `make deploy` to ship. No Mix, no Docker, no config files to manage.

## Quickstart

```bash
# 1. Install prerequisites (macOS)
brew install cmake elixir node binaryen

# 2. Install wasi-sdk (C → WASM compiler)
export V=25
curl -LO https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${V}/wasi-sdk-${V}.0-arm64-macos.tar.gz
tar xf wasi-sdk-*.tar.gz && mv wasi-sdk-${V}.0-arm64-macos ~/.wasi-sdk && rm wasi-sdk-*.tar.gz

# 3. Clone, build, run
git clone https://github.com/Jaketdaniels/elixir-workers.git
cd elixir-workers
make setup   # clones AtomVM, installs npm deps
make         # compiles everything
make dev     # http://localhost:8797
```

For x86 Mac or Linux, grab the matching wasi-sdk archive from the [releases page](https://github.com/WebAssembly/wasi-sdk/releases).

## Commands

| Command | What it does |
|---------|-------------|
| `make setup` | Clone AtomVM + install npm deps (first time only) |
| `make` | Build everything from scratch |
| `make app` | Rebuild just your Elixir code (fast — use while developing) |
| `make dev` | Start local dev server at http://localhost:8797 |
| `make deploy` | Deploy to Cloudflare (run `npx wrangler login` first) |
| `make clean` | Remove build artifacts |

## Writing your app

Your Elixir code lives in `elixir-app/lib/`. Drop `.ex` files anywhere in there — the build picks them up automatically. After editing, run `make app` to rebuild.

You need one module with a `start/0` function. That's your entry point — AtomVM calls it on every request.

### Adding a route

Open `elixir-app/lib/elixir_workers/router.ex` and add a function clause above the catch-all:

```elixir
def handle(%{"method" => "GET", "url" => "/api/hello"} = _req) do
  %{
    "status" => 200,
    "headers" => %{"content-type" => "application/json"},
    "body" => ElixirWorkers.JSON.encode(%{"greeting" => "hello from the edge"})
  }
end
```

Routes are pattern-matched top-to-bottom. The last clause returns a 404.

### Request / response

Your handler gets a map and returns a map:

```elixir
# Request
%{
  "method"  => "POST",
  "url"     => "/api/users?page=1",
  "headers" => %{"content-type" => "application/json"},
  "body"    => "{\"name\":\"Alice\"}"
}

# Response
%{
  "status"  => 201,
  "headers" => %{"content-type" => "application/json"},
  "body"    => ElixirWorkers.JSON.encode(%{"created" => true})
}
```

## Project layout

```
elixir-workers/
├── elixir-app/lib/          ← your code goes here
│   ├── elixir_workers.ex        entry point (start/0)
│   ├── elixir_workers/
│   │   ├── router.ex            routing
│   │   └── json.ex              JSON encoder/decoder
│   └── atomvm/wasi.ex           stdin/stdout bridge
├── worker/                  ← JS glue (don't touch)
├── atomvm-wasi/             ← C platform layer (don't touch)
├── scripts/                 ← build pipeline (don't touch)
└── Makefile
```

## License

Apache-2.0 — AtomVM is Apache-2.0 OR LGPL-2.1-or-later.
