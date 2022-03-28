(module
  (import "wasi_unstable" "fd_write" (func (param i32 i32 i32 i32) (result i32)))
  (func (export "_start")
    i32.const 8
    i32.const 16
    i32.store
    i32.const 8
    i32.const 14
    i32.store offset=4
    i32.const 1
    i32.const 8
    i32.const 1
    i32.const 4
    call 0
    drop)
  (memory (export "memory") 1)
  (data (i32.const 16) "Hello, W0rld!\0a"))