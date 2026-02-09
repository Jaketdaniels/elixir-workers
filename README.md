# elixir-workers

Write Elixir, deploy to Cloudflare Workers. Powered by AtomVM compiled to WebAssembly.

## Quickstart

**Prerequisites:** Elixir 1.17+ / Erlang/OTP 26+ and Node.js 18+.

```bash
# Install the project generator
mix archive.install hex elixir_workers_new

# Create a new project
mix elixir_workers.new my_app
cd my_app
mix deps.get

# Start dev server
mix elixir_workers.dev
```

Open http://localhost:8797.

## Your code

Define your app entry point and routes:

```elixir
# lib/my_app.ex
defmodule MyApp do
  use ElixirWorkers.App, router: MyApp.Router
end
```

```elixir
# lib/my_app/router.ex
defmodule MyApp.Router do
  use ElixirWorkers.Router

  defp routes do
    [
      {"GET", [], &page_home/1},
      {"GET", ["api", "health"], &api_health/1},
      {"POST", ["api", "echo"], &api_echo/1}
    ]
  end

  defp page_home(conn) do
    Conn.html(conn, 200, "<h1>Hello from the edge!</h1>")
  end

  defp api_health(conn) do
    Conn.json(conn, 200, %{"status" => "ok"})
  end

  defp api_echo(conn) do
    Conn.json(conn, 200, %{"echo" => conn["body"]})
  end
end
```

Edit your routes, run `mix elixir_workers.dev` to rebuild and serve.

`Enum`, `Map`, `GenServer`, `Supervisor`, pattern matching, protocols — it all works.

## Commands

| Command | Description |
|---------|-------------|
| `mix elixir_workers.new my_app` | Create a new project |
| `mix elixir_workers.build` | Compile Elixir and pack .avm |
| `mix elixir_workers.dev` | Build + start local dev server |
| `mix elixir_workers.deploy` | Build + deploy to Cloudflare |

## Deploy

```bash
npx wrangler login
mix elixir_workers.deploy
```

## Bindings (KV, D1)

Configure bindings in `worker/wrangler.jsonc`, then use them in your routes:

```elixir
# KV reads (transparent two-pass protocol)
{conn, value} = ElixirWorkers.KV.get(conn, "MY_KV", "user:1")

# KV writes (executed post-response)
conn = ElixirWorkers.KV.put(conn, "MY_KV", "user:1", "data")

# D1 queries
{conn, rows} = ElixirWorkers.D1.query(conn, "DB", "SELECT * FROM users WHERE id = ?", [1])
```

## Architecture

Three layers:
1. **worker/src/index.js** — JS Cloudflare Worker with full WASI runtime
2. **atomvm-wasi/** — C platform adapter compiled to WASM
3. **Your Elixir code** — compiled to .beam, packed into .avm archive

Each request creates a fresh WASM instance. The 559 KB `atomvm.wasm` runtime and stdlib .beam files are pre-built and distributed with the `elixir_workers` Hex package.

## Development (contributing)

To work on the framework itself (requires wasi-sdk, cmake, binaryen, python):

```bash
make setup    # Clone AtomVM, install deps
make          # Build everything
make priv     # Pre-compile stdlib for the package
```

## Project structure

```
packages/
  elixir_workers/      # Hex package: framework + Mix tasks + pre-built artifacts
  elixir_workers_new/  # Hex archive: project generator
atomvm-wasi/           # C platform adapter
vendor/AtomVM/         # AtomVM source (for rebuilding WASM + stdlib)
examples/hello_world/  # Example project
```

## License

Apache-2.0
