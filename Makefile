.PHONY: all atomvm app dev deploy clean setup check-deps

# Ensure Erlang is on PATH (Homebrew installs it separately)
export PATH := /opt/homebrew/opt/erlang/bin:$(PATH)

# Default: build everything
all: check-deps atomvm app

# ---- Setup ----

setup: check-deps
	@echo "Cloning AtomVM source..."
	@test -d vendor/AtomVM || git clone --depth 1 https://github.com/atomvm/AtomVM.git vendor/AtomVM
	@echo "Installing worker dependencies..."
	cd worker && npm install
	@echo "Setup complete."

check-deps:
	@command -v cmake >/dev/null 2>&1 || { echo "cmake required. Install: brew install cmake"; exit 1; }
	@command -v elixir >/dev/null 2>&1 || { echo "elixir required. Install: brew install elixir"; exit 1; }
	@command -v python3 >/dev/null 2>&1 || { echo "python3 required."; exit 1; }
	@echo "Dependencies OK"

# ---- AtomVM WASI build ----

atomvm: vendor/AtomVM
	@echo "Building AtomVM for WASI..."
	chmod +x scripts/build-atomvm.sh
	./scripts/build-atomvm.sh

vendor/AtomVM:
	git clone --depth 1 https://github.com/atomvm/AtomVM.git vendor/AtomVM

# ---- Elixir application ----

app:
	@echo "Building Elixir application..."
	chmod +x scripts/build-app.sh
	./scripts/build-app.sh

# ---- Development ----

dev: all
	cd worker && npx wrangler dev

# ---- Deployment ----

deploy: all
	cd worker && npx wrangler deploy

# ---- Cleanup ----

clean:
	rm -rf build/
	rm -f worker/atomvm.wasm
	rm -f worker/app.avm
	cd elixir-app && rm -rf _build deps

clean-all: clean
	rm -rf vendor/AtomVM
	cd worker && rm -rf node_modules
