# Nura

## Introduction

Nura is a small, statically typed functional language with closures, algebraic data types, pattern matching,
tail-call optimization, garbage collection, a REPL, and a built-in standard library.

## Installation

Linux/macOS (download the prebuilt binary):

```
curl -LO https://github.com/kyawyelin284/nura/releases/download/v1.0/nura-linux
chmod +x nura-linux
./nura-linux myprogram.nu
./nura-linux   # REPL
```

Optional one-line installer:

```
sh -c "$(curl -fsSL https://github.com/kyawyelin284/nura/releases/download/v1.0/install.sh)"
```

## Hello World

```
print("Hello, Nura!")
```

Note: Nura currently focuses on numeric and structural data. If string literals are not enabled in your build,
replace the above with `print(1)` to verify output.

## Variables and Expressions

```
let x = 10 in
let y = x * 2 in
print(y)
```

## Functions and Closures

```
let addMaker = fun x -> fun y -> x + y in
let add5 = addMaker(5) in
print(add5(3))
```

## Lists

```
let nums = [1,2,3,4] in
print(head(nums))
print(tail(nums))
```

## Algebraic Data Types & Pattern Matching

```
type Maybe a = Nothing | Just a

let value = Just(5) in
match value with
| Just(x) -> print(x)
| Nothing -> print(0)
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

## Modules

```
import "math.nu"
let result = add(2,3)
print(result)
```

Imports are merged into the current scope, so you can call imported functions directly.

## REPL Usage

```
$ nura
> let x = 10 in x * 2
20
> let f = fun y -> y + 5 in f(10)
15
```

## Standard Library

Built-ins:

- `print`
- `println`
- `head`
- `tail`
- `isEmpty`
- `length`

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
