.PHONY: all atomvm app dev deploy clean setup check-deps priv

export PATH := /opt/homebrew/opt/erlang/bin:$(PATH)

all: check-deps atomvm app

setup:
	chmod +x scripts/setup.sh
	./scripts/setup.sh

check-deps:
	@command -v cmake >/dev/null 2>&1 || { echo "Run 'make setup' first."; exit 1; }
	@command -v elixir >/dev/null 2>&1 || { echo "Run 'make setup' first."; exit 1; }
	@command -v python3 >/dev/null 2>&1 || { echo "Run 'make setup' first."; exit 1; }
	@test -d vendor/AtomVM || { echo "Run 'make setup' first."; exit 1; }

atomvm: vendor/AtomVM
	chmod +x scripts/build-atomvm.sh
	./scripts/build-atomvm.sh

vendor/AtomVM:
	git clone --depth 1 https://github.com/atomvm/AtomVM.git vendor/AtomVM

app:
	chmod +x scripts/build-app.sh
	./scripts/build-app.sh

dev: all
	cd worker && npx wrangler dev

deploy: all
	cd worker && npx wrangler deploy

# Pre-compile stdlib and bundle artifacts into the framework package
priv: check-deps
	@echo "==> Pre-compiling stdlib for packages/elixir_workers/priv/"
	@mkdir -p packages/elixir_workers/priv/stdlib
	@# Compile Erlang stdlib
	@for f in vendor/AtomVM/libs/estdlib/src/*.erl; do \
		erlc -o packages/elixir_workers/priv/stdlib/ "$$f" 2>/dev/null || true; \
	done
	@# Compile Elixir stdlib (exclude hardware modules)
	@for f in vendor/AtomVM/libs/exavmlib/lib/*.ex; do \
		base=$$(basename "$$f"); \
		case "$$base" in \
			gpio.ex|i2c.ex|ledc.ex|avm_port.ex|console.ex) continue ;; \
		esac; \
		elixirc --no-deps-check -o packages/elixir_workers/priv/stdlib/ "$$f" 2>/dev/null || true; \
	done
	@# Copy atomvm.wasm
	@if [ -f worker/atomvm.wasm ]; then \
		cp worker/atomvm.wasm packages/elixir_workers/priv/atomvm.wasm; \
		echo "Copied atomvm.wasm"; \
	else \
		echo "Warning: worker/atomvm.wasm not found. Run 'make atomvm' first."; \
	fi
	@echo "==> Done. Artifacts in packages/elixir_workers/priv/"

clean:
	rm -rf build/
	rm -f worker/atomvm.wasm worker/app.avm

clean-all: clean
	rm -rf vendor/AtomVM
	cd worker && rm -rf node_modules
