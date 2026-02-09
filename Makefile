.PHONY: all atomvm app dev deploy clean setup check-deps

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

clean:
	rm -rf build/
	rm -f worker/atomvm.wasm worker/app.avm

clean-all: clean
	rm -rf vendor/AtomVM
	cd worker && rm -rf node_modules
