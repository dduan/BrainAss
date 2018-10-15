compile-compiler:
	ghc compile.hs

compile-wasm:
	wat2wasm main.wat -o main.wasm

serve:
	python -m SimpleHTTPServer

clean:
	rm -f compile.hs main.wasm
