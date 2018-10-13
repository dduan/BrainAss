(module
  (func $getchar (import "bf" "getchar") (result i32))
  (func $putchar (import "bf" "putchar") (param i32))
  (memory 1)
  (func $main
    (local $p i32)
    (set_local $p (i32.const 0))
  )
  (export "bf" (func $main))
)
