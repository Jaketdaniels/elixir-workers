# elixir-workers

Write Elixir, deploy to Cloudflare Workers.

## Get started

```bash
git clone https://github.com/Jaketdaniels/elixir-workers.git
cd elixir-workers
make setup
make
make dev
```

Open http://localhost:8797.

## Deploy

```bash
npx wrangler login
make deploy
```

## Your code

All your Elixir lives in `elixir-app/lib/`. Add routes in `router.ex`, run `make app` to rebuild.

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
