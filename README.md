# Wala

A toy programming language mixing many inspirations *(mainly Zig, Python and F#)*.

Pronounced `\vwa.la\` as in french voila *(meaning here it is)*. The exact acronym's composition is unspecified but can be interpreted as [WebAssembly](https://webassembly.org/) Language Adapter or What Another Linguistic Anomaly.

```
fib =: (x) ->
    x < 3 ? x :
        fib. (x - 1) +: fib. (x - 2)

fib. 20
```

## Philosophy 

Wala tries to simply Object Oriented Programming syntax

Get bob's student number and display it:
- Python: `print(students.get("bob").id)`
- Wala: `students get "bob" .id print.`


It helps functional composition with left to right evaluation and [pipeline operator](https://bradcollins.com/2015/04/03/f-friday-pipeline-operators/).

Display Top 3 students ratings means:
- Python
```python
ratings = (value for student in students
  for name, value in student.ratings)
grades = (sum(rating)/len(rating) for rating in ratings)
print(sorted(list(grades), revert=True)[:3])
```
- Wala
```
students map
  |> .ratings mean: (r) -> r .value
|> sorted true take 3
|> print.
```

## Features

* Basic parser
* Basic interpreter

### Planned

* WASM Codegen
* LLVM Codegen
* OOP
* Iterators

## Install

### Prerequisites

- [Zig](https://ziglang.org/learn/getting-started)
- [Zigmod](https://nektro.github.io/zigmod/)

### Building

```sh
git clone https://github.com/CalmSystem/wala.git
cd wala
zigmod ci
zig build -Drelease-safe
PATH=$PATH:./zig-out/bin
```

## Usage

* Evaluate `samples/fib.wal`
```sh
wala run samples/fib.wal
```

## License

Distributed under the MIT license to facilitate cooperation and knowledge sharing. However, use with respect to contributors and end-users is strongly advised. See [LICENSE](LICENSE) for more information.
