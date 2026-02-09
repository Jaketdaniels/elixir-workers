# elixir-workers

Run Elixir on Cloudflare Workers via AtomVM compiled to WebAssembly.

```
HTTP Request
     |
     v
┌─────────────────────────────────────────┐
│  Cloudflare Worker (JS)                 │
│  Serializes request to JSON on stdin    │
├─────────────────────────────────────────┤
│  AtomVM  (WASM/WASI)                   │
│  Tiny BEAM VM compiled from C to WASM   │
├─────────────────────────────────────────┤
│  Your Elixir App  (.avm archive)        │
│  Pattern-matched routing, JSON codec    │
└─────────────────────────────────────────┘
     |
     v
HTTP Response
```

AtomVM is a lightweight BEAM virtual machine written in C. This project retargets it from its usual embedded platforms (ESP32, STM32) to WASI, allowing it to run inside Cloudflare Workers' V8 isolates as a WebAssembly module.

Your Elixir code compiles to standard `.beam` bytecode, gets packaged into an `.avm` archive, and runs on the edge — pattern matching, immutable data, fault tolerance and all.

## Quick start

### Prerequisites

| Tool | Version | Install |
|------|---------|---------|
| wasi-sdk | v24+ | [github.com/WebAssembly/wasi-sdk](https://github.com/WebAssembly/wasi-sdk/releases) → extract to `~/.wasi-sdk/` |
| Elixir | 1.17+ | `brew install elixir` |
| Erlang/OTP | 26+ | installed with Elixir |
| CMake | 3.20+ | `brew install cmake` |
| Node.js | 18+ | `brew install node` |

### Build and run

```bash
git clone https://github.com/youruser/elixir-workers.git
cd elixir-workers

make setup    # Clone AtomVM, install npm deps
make atomvm   # Compile AtomVM to WASM (~559KB optimized binary)
make app      # Compile Elixir → .beam → .avm (~12KB archive)
make dev      # Start dev server on localhost:8797
```

### Test it

```bash
# HTML page
curl http://localhost:8797/

# JSON health check
curl http://localhost:8797/api/health
# {"status":"ok","runtime":"atomvm-wasi","platform":"cloudflare-workers"}

# Echo endpoint
curl -X POST -d '{"hello":"world"}' http://localhost:8797/api/echo

# 404
curl http://localhost:8797/nope
# {"error":"not_found","method":"GET","path":"/nope"}
```

### Deploy to Cloudflare

```bash
make deploy
```

## How it works

1. **Build time**: AtomVM's C source is compiled to a `.wasm` binary using wasi-sdk. Your Elixir code is compiled to `.beam` files using standard `elixirc`, then stripped and packaged into an `.avm` archive alongside AtomVM's stdlib.

2. **Request time**: The JS Worker receives an HTTP request, serializes it as JSON, and pipes it to the WASM module's stdin. AtomVM boots, loads the `.avm`, finds the startup module (`ElixirWorkers`), and calls `start/0`.

3. **Elixir handles it**: Your Elixir code reads the JSON request from stdin via a NIF, routes it through pattern-matched function heads, and writes a JSON response to stdout via another NIF.

4. **Response**: The JS Worker captures stdout, parses the JSON, and returns a proper HTTP Response.

The entire cycle runs per-request. WASM compilation is cached by the runtime between requests.

## Project structure

```
elixir-workers/
├── atomvm-wasi/              C platform adapter for AtomVM on WASI
│   ├── src/
│   │   ├── sys.c             Platform interface (sys.h implementation)
│   │   ├── main.c            WASI entry point — loads .avm, runs VM
│   │   ├── platform_nifs.c   NIFs: read_stdin, write_stdout, platform
│   │   └── platform_defaultatoms.c
│   ├── include/
│   │   ├── wasi_sys.h        Platform data structures
│   │   ├── wasi_compat.h     Stubs for missing WASI functions
│   │   └── avm_version.h     Version header
│   └── CMakeLists.txt        Build configuration
│
├── worker/                   Cloudflare Worker
│   ├── src/
│   │   └── index.js          WASI runtime + HTTP bridge (vanilla JS)
│   ├── wrangler.jsonc        Worker configuration
│   ├── atomvm.wasm           (built) AtomVM binary
│   └── app.avm               (built) Elixir application archive
│
├── elixir-app/               Elixir application
│   └── lib/
│       ├── elixir_workers.ex           Entry point — start/0
│       ├── elixir_workers/router.ex    HTTP router
│       ├── elixir_workers/json.ex      JSON encoder/decoder
│       └── atomvm/wasi.ex              NIF bindings
│
├── scripts/
│   ├── build-atomvm.sh       Compiles AtomVM C → WASM
│   ├── build-app.sh          Compiles Elixir/Erlang → .avm
│   └── pack_avm.py           AVM archive creator
│
├── vendor/AtomVM/            AtomVM source (cloned by make setup)
├── build/                    Build artifacts
├── Makefile                  Top-level build
└── CLAUDE.md                 AI assistant instructions
```

## Writing your application

Edit the files in `elixir-app/lib/`. The entry point is `ElixirWorkers.start/0`.

### Adding routes

Edit `elixir-app/lib/elixir_workers/router.ex`:

```elixir
defmodule ElixirWorkers.Router do
  def handle(%{"method" => "GET", "url" => "/api/hello"} = _req) do
    %{
      "status" => 200,
      "headers" => %{"content-type" => "application/json"},
      "body" => ElixirWorkers.JSON.encode(%{"hello" => "world"})
    }
  end

  # Catch-all 404
  def handle(%{"method" => method, "url" => url} = _req) do
    %{
      "status" => 404,
      "headers" => %{"content-type" => "application/json"},
      "body" => ElixirWorkers.JSON.encode(%{"error" => "not_found"})
    }
  end
end
```

After editing, rebuild with `make app`.

### Request format

Your Elixir code receives a map with string keys:

```elixir
%{
  "method"  => "POST",
  "url"     => "/api/users?page=1",
  "headers" => %{"content-type" => "application/json", "host" => "example.com"},
  "body"    => "{\"name\":\"Alice\"}"
}
```

### Response format

Return a map with `"status"`, `"headers"`, and `"body"`:

```elixir
%{
  "status"  => 201,
  "headers" => %{"content-type" => "application/json"},
  "body"    => ElixirWorkers.JSON.encode(%{"created" => true})
}
```

## Constraints

This runs on AtomVM, not full OTP. Key differences:

- **Single-threaded** — no processes, no `spawn`, no message passing (CF Workers are single-threaded)
- **No OTP applications** — no supervisors, no GenServers, no application trees
- **No networking** — no `:gen_tcp`, `:httpc`, etc. (all I/O goes through the stdin/stdout bridge)
- **No file system** — WASI provides a virtual filesystem with only the `.avm` file
- **Minimal stdlib** — only 5 modules bundled (maps + 4 app modules); add more from `vendor/AtomVM/libs/` as needed
- **No string interpolation** — avoid `"hello #{name}"` syntax as it pulls in the `String.Chars` protocol. Use binary concatenation: `<<"hello ", name::binary>>`

What **does** work:
- Pattern matching (the full power of it)
- Maps, lists, tuples, binaries
- Recursion, guards, multi-clause functions
- Modules, structs
- Most `:erlang`, `:lists`, `:binary`, `:maps` functions (built-in NIFs/BIFs)
- The `ElixirWorkers.JSON` module for encoding/decoding

## Sizes

| Component | Size |
|-----------|------|
| AtomVM WASM binary | ~559 KB |
| Application .avm (5 modules) | ~12 KB |
| Total Worker bundle | ~571 KB |

The WASM binary is built with `-Oz` + LTO + `wasm-opt -Oz` for aggressive size optimization. The .avm is aggressively pruned — most `:erlang`, `:lists`, `:binary` functions are NIFs/BIFs built into the WASM and don't need .beam files.

For comparison, the Cloudflare Workers size limit is 10 MB (paid) / 1 MB (free). This fits comfortably in the free tier.

## License

Apache-2.0

AtomVM is licensed under Apache-2.0 OR LGPL-2.1-or-later.
