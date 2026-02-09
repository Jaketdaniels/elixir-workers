# elixir-workers

Write Elixir, deploy to Cloudflare Workers.

## Install

You'll need [Homebrew](https://brew.sh) and a free [Cloudflare account](https://dash.cloudflare.com/sign-up).

**1. Install dependencies**

```bash
brew install cmake elixir node binaryen
```

**2. Install the WebAssembly compiler**

```bash
export V=25
curl -LO https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${V}/wasi-sdk-${V}.0-arm64-macos.tar.gz
tar xf wasi-sdk-*.tar.gz && mv wasi-sdk-${V}.0-arm64-macos ~/.wasi-sdk && rm wasi-sdk-*.tar.gz
```

On x86 Mac or Linux, grab the matching archive from the [wasi-sdk releases](https://github.com/WebAssembly/wasi-sdk/releases).

**3. Clone and build**

```bash
git clone https://github.com/Jaketdaniels/elixir-workers.git
cd elixir-workers
make setup
make
```

**4. Start developing**

```bash
make dev
```

Open http://localhost:8797. Edit your Elixir code in `elixir-app/lib/`, run `make app` to rebuild.

**5. Deploy**

```bash
npx wrangler login
make deploy
```

## Your code

All your Elixir lives in `elixir-app/lib/`. Add routes in `router.ex`:

```elixir
def handle(%{"method" => "GET", "url" => "/api/hello"} = _req) do
  %{
    "status" => 200,
    "headers" => %{"content-type" => "application/json"},
    "body" => ElixirWorkers.JSON.encode(%{"greeting" => "hello from the edge"})
  }
end
```

`Enum`, `Map`, `GenServer`, `Supervisor`, pattern matching, protocols — it all works.

## License

Apache-2.0
