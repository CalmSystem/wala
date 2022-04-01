(module
  (import "wasi_unstable" "fd_write" (func (;0;) (type 0)))
  (func (;1;) (type 1)
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
    call $jmp
    drop)
  (func $jmp (;2;) (type $t_print)
    (param $a i32)
    (param i32 i32)
    (param $d i32)
    ;; test for out of order functions
    local.get $a
    local.get 1
    local.get 2
    local.get $d
    call 0)
  (type $t_print (;0;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;1;) (func))
  (export "memory" (memory 0))
  (memory (;0;) 1)
  (export "_start" (func 1))
  (data (i32.const 16) "Hello, W0rld!\0a"))