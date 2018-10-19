SHELL = /bin/bash

build-and-run-helloworld: compile-compiler
	cat hello.bf | ./compile bf.getchar bf.putchar bf > main.wat
	wat2wasm main.wat -o main.wasm
	$(MAKE) serve

compile-compiler:
	ghc compile.hs

serve:
	python -m SimpleHTTPServer

clean:
	rm -f compile.hs main.wasm
