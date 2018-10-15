(module
  (func $getchar (import "bf" "getchar") (result i32))
  (func $putchar (import "bf" "putchar") (param i32))
  (memory 1)
  (global $p (mut i32) (i32.const 0))
  (func $right
    get_global $p
    i32.const 1
    i32.add
    set_global $p
  )
  (func $left
    get_global $p
    i32.const -1
    i32.add
    set_global $p
  )
  (func $inc
    get_global $p
    get_global $p
    i32.load
    i32.const 1
    i32.add
    i32.store
  )
  (func $dec
    get_global $p
    get_global $p
    i32.load
    i32.const -1
    i32.add
    i32.store
  )
  (func $main
    (local $i i32)
    (local $i1 i32)
    (set_local $i (i32.const 5))
    (block
      (loop
        get_local $i
        i32.const 0
        i32.eq
        br_if 1

        (set_local $i1 (i32.const 5))
        (block
          (loop
            get_local $i1
            i32.const 0
            i32.eq
            br_if 1

            i32.const 42
            call $putchar

            get_local $i1
            i32.const -1
            i32.add
            set_local $i1
            br 0
          )
        )

        get_local $i
        i32.const -1
        i32.add
        set_local $i
        br 0
      )
    )
  )
  (export "bf" (func $main))
)
