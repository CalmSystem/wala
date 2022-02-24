(module
  (func $fib (export "fib") (param $n i64) (result i64)
    (if (result i64) (i64.le_u (local.get $n) (i64.const 1))
      (i64.const 1)
      (i64.add
        (call $fib (i64.sub (local.get $n) (i64.const 2)))
        (call $fib (i64.sub (local.get $n) (i64.const 1)))))))
