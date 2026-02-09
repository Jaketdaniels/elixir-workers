.PHONY: all atomvm dev deploy clean setup check-deps priv

export PATH := /opt/homebrew/opt/erlang/bin:$(PATH)

STARTER_DIR := _build/starter
ATOMVM_WASM := build/atomvm-wasi/atomvm.wasm

all: check-deps $(ATOMVM_WASM) $(STARTER_DIR)

setup:
	@NONINTERACTIVE=$(NONINTERACTIVE) ./scripts/setup.sh

check-deps:
	@command -v cmake >/dev/null 2>&1 || { echo "Run 'make setup' first."; exit 1; }
	@command -v elixir >/dev/null 2>&1 || { echo "Run 'make setup' first."; exit 1; }
	@command -v python3 >/dev/null 2>&1 || { echo "Run 'make setup' first."; exit 1; }
	@test -d vendor/AtomVM || { echo "Run 'make setup' first."; exit 1; }

# Real file target — only rebuilds if the WASM binary is missing
$(ATOMVM_WASM): vendor/AtomVM
	@chmod +x scripts/build-atomvm.sh
	@./scripts/build-atomvm.sh

# Convenience alias
atomvm: $(ATOMVM_WASM)

vendor/AtomVM:
	git submodule update --init --depth 1 vendor/AtomVM

# Pre-compile stdlib and bundle atomvm.wasm into the framework package.
# Uses a sentinel file so this only runs once (use 'make clean' to force rebuild).
# Order-only prerequisite (|) for check-deps so it doesn't invalidate the sentinel.
priv: packages/elixir_workers/priv/.built

packages/elixir_workers/priv/.built: $(ATOMVM_WASM) | check-deps
	@echo "==> Pre-compiling stdlib for packages/elixir_workers/priv/"
	@mkdir -p packages/elixir_workers/priv/stdlib
	@# Compile Erlang stdlib
	@for f in vendor/AtomVM/libs/estdlib/src/*.erl; do \
		erlc -o packages/elixir_workers/priv/stdlib/ "$$f" 2>/dev/null || true; \
	done
	@# Compile Elixir stdlib (all at once for interdependency resolution, exclude hardware modules)
	@EXAVMLIB_FILES=""; \
	for f in vendor/AtomVM/libs/exavmlib/lib/*.ex; do \
		base=$$(basename "$$f"); \
		case "$$base" in \
			GPIO.ex|I2C.ex|LEDC.ex|AVMPort.ex|Console.ex) continue ;; \
		esac; \
		EXAVMLIB_FILES="$$EXAVMLIB_FILES $$f"; \
	done; \
	elixirc -o packages/elixir_workers/priv/stdlib/ $$EXAVMLIB_FILES 2>/dev/null || true
	@# Copy atomvm.wasm from build output
	@if [ -f build/atomvm-wasi/atomvm.wasm ]; then \
		cp build/atomvm-wasi/atomvm.wasm packages/elixir_workers/priv/atomvm.wasm; \
		echo "Copied atomvm.wasm"; \
	else \
		echo "Error: build/atomvm-wasi/atomvm.wasm not found. Run 'make atomvm' first."; \
		exit 1; \
	fi
	@touch $@
	@echo "==> Done. Artifacts in packages/elixir_workers/priv/"

# Generate a starter project from the template (mirrors the user experience)
$(STARTER_DIR): packages/elixir_workers/priv/.built
	@echo "==> Generating starter project from template..."
	@mkdir -p $(STARTER_DIR)/lib/starter
	@# Render Elixir source from templates
	@sed 's/<%= @app_module %>/Starter/g; s/<%= @app_name %>/starter/g' \
		packages/elixir_workers/priv/templates/app.ex.eex > $(STARTER_DIR)/lib/starter.ex
	@sed 's/<%= @app_module %>/Starter/g' \
		packages/elixir_workers/priv/templates/router.ex.eex > $(STARTER_DIR)/lib/starter/router.ex
	@# mix.exs with path dep to framework package
	@echo 'defmodule Starter.MixProject do' > $(STARTER_DIR)/mix.exs
	@echo '  use Mix.Project' >> $(STARTER_DIR)/mix.exs
	@echo '  def project do' >> $(STARTER_DIR)/mix.exs
	@echo '    [app: :starter, version: "0.1.0", elixir: "~> 1.17", start_permanent: false, deps: deps()]' >> $(STARTER_DIR)/mix.exs
	@echo '  end' >> $(STARTER_DIR)/mix.exs
	@echo '  def application, do: [extra_applications: []]' >> $(STARTER_DIR)/mix.exs
	@echo '  defp deps do' >> $(STARTER_DIR)/mix.exs
	@echo '    [{:elixir_workers, path: "../../packages/elixir_workers"}]' >> $(STARTER_DIR)/mix.exs
	@echo '  end' >> $(STARTER_DIR)/mix.exs
	@echo 'end' >> $(STARTER_DIR)/mix.exs
	@# Workers config
	@sed 's/<%= @app_name %>/starter/g; s/<%= @port %>/8797/g' \
		packages/elixir_workers/priv/templates/wrangler.jsonc.eex > $(STARTER_DIR)/wrangler.jsonc
	@sed 's/<%= @app_name %>/starter/g' \
		packages/elixir_workers/priv/templates/package.json.eex > $(STARTER_DIR)/package.json
	@printf '/_build/\n/deps/\n/node_modules/\n*.ez\n.elixir_ls/\n' > $(STARTER_DIR)/.gitignore
	@cd $(STARTER_DIR) && mix deps.get --no-deps-check

dev: $(STARTER_DIR)
	cd $(STARTER_DIR) && mix elixir_workers.dev

deploy: $(STARTER_DIR)
	cd $(STARTER_DIR) && mix elixir_workers.deploy

clean:
	rm -rf build/atomvm-wasi
	rm -f packages/elixir_workers/priv/.built
	@# Clean build artifacts inside the starter project, but preserve the project itself
	rm -rf $(STARTER_DIR)/_build
	rm -rf $(STARTER_DIR)/deps

clean-all: clean
	rm -rf $(STARTER_DIR)
	rm -rf vendor/AtomVM
