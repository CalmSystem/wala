(module
  (import "wasi_unstable" "fd_write"
    (func $fd_write
      (param i32 i32 i32 i32)
      (result i32)))
  (;
  
comment

  ;)
  (memory (export "memory") 1)

  (data (i32.const 16) "Hello, W0rld!\n")

  (func $main (export "_start")
    (i32.store (i32.const 8) (i32.const 16))
    (i32.store offset=4 (i32.const 8) (i32.const 14))
    (call $fd_write
      (i32.const 1) ;; fd: stdout
      (i32.const 8) ;; const ciovec*
      (i32.const 1) ;; len of ciovec
      (i32.const 4)) ;; written*
    drop))
