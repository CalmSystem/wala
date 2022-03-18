# Wala

A language trying to simplify [WebAssembly Text](https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format) syntax while keeping the full expressiveness and retro-compatibility. Mixing many inspirations *(mainly Python and Racket)*.

Pronounced `\vwa.la\` as in french voila *(meaning here it is)*. The exact acronym's composition is unspecified but can be interpreted as [WebAssembly](https://webassembly.org/) Language Adaptor or What Another Linguistic Anomaly.

## Philosophy 

It is implemented as a set of complementary extensions over standard [WebAssembly Text Format](https://webassembly.github.io/spec/core/text/index.html)

```wal
func $fib (export)
  u64 $n
  if {($n) <= 2}
    1
    +
      $fib{($n) - 2}
      $fib{($n) - 1}

```

Is expended to:
```wat
(module
  (func $fib (export "fib") (param $n i64) (result i64)
    (if (result i64) (i64.le_u (local.get $n) (i64.const 2))
      (i64.const 1)
      (i64.add
        (call $fib (i64.sub (local.get $n) (i64.const 2)))
        (call $fib (i64.sub (local.get $n) (i64.const 1)))))))
```

## Install

### Prerequisites

- [Zig](https://ziglang.org/learn/getting-started)
- [Zigmod](https://nektro.github.io/zigmod/)

#### Optional

- [Binaryen](https://github.com/WebAssembly/binaryen) for additional optimizations
- [Wasmtime](https://github.com/bytecodealliance/wasmtime) for WASI runtime

### Building

```sh
git clone https://github.com/CalmSystem/wala.git
cd wala
zigmod ci
zig build -Drelease-safe
PATH=$PATH:./zig-out/bin
```

## Usage

* Run built module *(requires `wat2wasm` and `wasmtime`)*
```sh
wala run samples/hello.wala
```
* Convert `test/fib.wala` to Wat
```sh
wala build samples/fib.wala
```

## Features

### Sweet expression

Deducing parentheses from indentation based on [Readable Lisp S-expressions Project](https://readable.sourceforge.io/)'s [Spir110](https://srfi.schemers.org/srfi-110/srfi-110.html)

#### Curly infix expressions

- `{"hello" upper}` -> `(upper "hello)`
- `{a + b}` -> `(+ a b)`
- `{a * b * c}` -> `(* a b c)`
- `{$fn call a b c}` -> `(call $fb a b c}`

#### Neoteric expression

Like function calls

- `cos(v)` -> `(cos v)`
- `e()` -> `(e)`
- `sum(a b c)` -> `(sum a b c)`
- `f{n + 1}` -> `(f (+ n 1))`

#### Short const

- `42i32` -> `(i32.const 42)`
<!-- TODO: - `1.3f64` -> `(f64.const 1.3)` -->

#### Operand Type Deduction

- `(i32.add 35 7)` -> `(i32.add (i32.const 35) (i32.const 7))`
- `(i64.add 35 7)` -> `(i64.add (i64.const 35) (i64.const 7))`

#### Common Operators

- `(+ 35i32 7)` -> `(i32.add (i32.const 35) (i32.const 7))`
- `(+ 35i64 7)` -> `(i64.add (i64.const 35) (i64.const 7))`

#### Ident expansion

- `($a_func ($a_param) ($a_global))` -> `(call $a_func (local.get $a_param) (global.get $a_global)`

### Planned

#### Short var

- `$var@l` -> `(local.get $var)`
- `$log@g` -> `(global.get $log)`

#### Result Type Deduction

No need to specify blocks and function result types

#### Interface integration

Allows to define, import and use `wit` declarations. See [fib.wasi.wal](./test/wala/fib.wasi.wal)

#### Tagged types

- `{42s64 <= 1}` `i64@s` -> `(i64.le_s (i64.const 42) (i64.const 1))`
- `{42u64 <= 1}` `i64@u` -> `(i64.le_u (i64.const 42) (i64.const 1))`

## License

Distributed under the MIT license to facilitate cooperation and knowledge sharing. However, use with respect to contributors and end-users is strongly advised. See [LICENSE](LICENSE) for more information.
