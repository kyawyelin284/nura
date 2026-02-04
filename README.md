# Nura

## Introduction

Nura is a small, statically typed functional language with closures, algebraic data types, pattern matching,
tail-call optimization, garbage collection, a REPL, and a built-in standard library.

## Installation

Linux/macOS (download the prebuilt binary):

```
curl -LO https://github.com/kyawyelin284/nura/releases/download/v1.0.1/nura-linux
chmod +x nura-linux
./nura-linux myprogram.nu
./nura-linux   # REPL
```

Optional one-line installer:

```
sh -c "$(curl -fsSL https://github.com/kyawyelin284/nura/releases/download/v1.0.1/install.sh)"
```

Direct download links:

- https://github.com/kyawyelin284/nura/releases/download/v1.0.1/nura-linux
- https://github.com/kyawyelin284/nura/releases/download/v1.0.1/install.sh

## Hello World

```
print("Hello, Nura!")
```

Output:

```
Hello, Nura!
```

## Variables and Expressions

```
let x = 10 in
let y = x * 2 in
print(y)
```

Output:

```
20
```

## Functions and Closures

```
let addMaker = fun x -> fun y -> x + y in
let add5 = addMaker(5) in
print(add5(3))
```

Output:

```
8
```

Multi-argument calls are supported with commas and desugar to nested applications:

```
let add = fun x -> fun y -> x + y in
print(add(2, 3))
```

Output:

```
5
```

## Strings and Comparisons

```
print("Hello, " + "Nura!")
print(strlen("Nura"))
print("b" > "a")
print("a" <= "a")
print([1,2,3] < [1,2,4])
type Maybe a = Nothing | Just a
print(Just(1) < Just(2))
```

Output:

```
Hello, Nura!
4
true
true
true
true
```

## Lists

```
let nums = [1,2,3,4] in
print(head(nums))
print(tail(nums))
```

Output:

```
1
[2, 3, 4]
```

## Algebraic Data Types & Pattern Matching

```
type Maybe a = Nothing | Just a

let value = Just(5) in
match value with
| Just(x) -> print(x)
| Nothing -> print(0)
```

Output:

```
5
```

## Recursive Functions (Tail-Call Optimized)

```
let rec sumList lst acc =
  match lst with
  | [] -> acc
  | x:xs -> sumList(xs, acc + x)
in
print(sumList([1,2,3,4], 0))
```

Output:

```
10
```

## Modules

```
import "math.nu"
let result = add(2,3)
print(result)
```

Output:

```
5
```

Imports are merged into the current scope, so you can call imported functions directly. Comma-separated
calls like `add(2, 3)` are equivalent to `add(2)(3)`.

## REPL Usage

```
$ nura
nura> let x = 10 in x * 2
20
nura> let f = fun y -> y + 5 in f(10)
15
nura> let add = fun a -> fun b -> a + b in add(2, 3)
5
```

## Standard Library

Built-ins:

- `print`
- `println`
- `head`
- `tail`
- `isEmpty`
- `length`
- `strlen`

## Architecture

The pipeline is:

```
Parser → Type Checker → Compiler → VM → GC
```

## Garbage Collection

The VM uses a mark-and-sweep collector:

- **Roots**: VM stack values, current frame environment, call frame environments, and global environment.
- **Mark**: Traverse reachable heap objects (closures, cons cells, constructor objects) and set their mark flag.
- **Sweep**: Remove unmarked objects and reset marks on survivors.
- **Trigger**: Runs automatically before allocation when heap size exceeds a threshold.

## Tail-Call Optimization

When the compiler detects a call in tail position (the last expression in a function body, or the final
expression in both branches of an `if`), it emits `TailCall` instead of `Call`.

The VM executes `TailCall` by reusing the current frame rather than pushing a new one. This prevents stack
growth for tail-recursive functions.

## Features Recap

Closures, ADTs, pattern matching, tail-call optimization, garbage collection, REPL, standard library.
