# *Brain*fuck to Web*Ass*embly compiler

This is a Brainfuck to WebAssembly compiler.

To use it, you'll need a recent [GHC][0] and [wat2wasm][1] installed in your
environment.

Run `make compile-compiler` (or `ghc brainass.hs`) to compile the compiler.

Run `./brainass` (or whatever you compiled `brainass.hs` to) to read how to use
the compiler. It should print the following.

```
Usage: compile IMPORTED_INPUT IMPORTED_OUTPUT EXPORT
where
  IMPORTED_INPUT  is imported Javascript function for Brainfuck's ,
  IMPORTED_OUTPUT is imported Javascript function for Brainfuck's .
  EXPORT is the exported Javascript function from the WASM module

Brainfuck source is read fro STDIN and text wasm is written to STDOUT
(similar to `cat`)

Example:
  cat source.bf > compile bf.getchar bf.putchar bf > main.wat
  makes wasm module import bf.getchar as "," and bf.putchar as "."
  and the wasm module exports a function named bf.
```

Running `make build-and-run-helloworld` will:
1. build the compiler in local directory as `./compile`.
2. compile a hello world program in Brainfuck to main.wasm
3. launch a server at http://localhost:8000/play.html which runs the wasm
   program

You can read this make command, as well as `play.{js,html}` to learn how to use
the compiler as well.

[0]: https://www.haskell.org/ghc/
[1]: https://github.com/WebAssembly/wabt#running-wat2wasm-and-wast2json
