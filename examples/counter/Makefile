.PHONY: build compose clean

# Build the Yel component and generate WIT
build:
	cargo build

# Build the business logic component (uses wit_bindgen::generate! macro)
logic:
	cargo build --lib --target wasm32-wasip2

# Compose UI and logic components with wac
compose:
	@echo "Composing UI component (counter-ui.wasm) with logic component..."
	wac plug counter-ui.wasm --plug build/counter_logic.wasm -o final-counter.wasm                        
	@echo "Final composed component: counter-app.wasm"

# Build everything and output final WASM
all: compose
	@echo ""
	@echo "=========================================="
	@echo "Final WASM component: counter-app.wasm"
	@echo "=========================================="

clean:
	cargo clean
	rm -f counter-ui.wasm counter-app.wasm
	rm -rf wit/counter.wit
