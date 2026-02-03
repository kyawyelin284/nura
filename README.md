# Nura

Nura is a small, statically typed functional language with a reference interpreter and a bytecode VM backend.

## Language Features

- Integers and booleans
- Arithmetic: `+`, `-`, `*`
- Comparisons: `>`
- Variables and `let` / `let rec`
- Functions and application: `fun x -> expr`, `f(arg)`
- Conditionals: `if cond then e1 else e2`
- Lists: `[]`, `[e1, e2, ...]`, cons patterns
- Algebraic data constructors: `Just(1)`, `Node(Leaf(1), Leaf(2))`
- Pattern matching:
  - `match expr with | [] -> e1 | x:xs -> e2`
  - Constructor patterns: `Just(x)`, `Nothing`, `Node(l, r)`
- Builtins: `print`, `println`, `head`, `tail`, `isEmpty`, `length`

## Example Programs

Factorial (tail recursion via `let rec`):

```
let rec fact = fun n ->
  if n > 1 then n * fact(n - 1) else 1
in print(fact(5))
```

List match:

```
match [1,2,3] with | [] -> 0 | x:xs -> x
```

Constructor patterns:

```
match Just(5) with | Just(x) -> print(x) | Nothing -> print(0)
```

## Architecture

The pipeline is:

```
Parser → Type Checker → Compiler → VM → GC
```

- **Parser**: Megaparsec-based parser that builds an AST.
- **Type Checker**: Hindley-Milner-style inference with unification and type variables.
- **Compiler**: Lowers AST into stack-based bytecode with closures and pattern matching.
- **VM**: Executes bytecode with an environment, call frames, and a heap.
- **GC**: Mark-and-sweep over heap objects.

## Running Programs

Build:

```
stack build
```

### Install Without Haskell/Stack

Linux/macOS (download the prebuilt binary):

```
curl -LO https://github.com/kyawyelin284/nura/releases/download/v1.0/nura-linux
chmod +x nura-linux
./nura-linux myprogram.nu
./nura-linux   # starts REPL
```

Optional one-line install:

```
curl -LO https://github.com/kyawyelin284/nura/releases/download/v1.0/nura-linux && chmod +x nura-linux
```

Cross-platform one-line installer:

```
sh -c "$(curl -fsSL https://github.com/kyawyelin284/nura/releases/download/v1.0/install.sh)"
```

Run a file (interpreter backend):

```
stack exec nura-exe -- path/to/file.nu
```

Run with VM backend:

```
stack exec nura-exe -- --vm path/to/file.nu
```

REPL (no file provided):

```
stack exec nura-exe
```

## Garbage Collection

The VM uses a mark-and-sweep collector.

- **Roots**: VM stack values, current frame environment, call frame environments, and global environment.
- **Mark**: Traverse reachable heap objects (closures, cons cells, constructor objects) and set their mark flag.
- **Sweep**: Remove unmarked objects and reset marks on survivors.
- **Trigger**: Runs automatically before allocation when heap size exceeds a threshold.

## Tail-Call Optimization

When the compiler detects a call in tail position (the last expression in a function body, or the final
expression in both branches of an `if`), it emits `TailCall` instead of `Call`.

The VM executes `TailCall` by reusing the current frame rather than pushing a new one. This prevents stack
growth for tail-recursive functions.
# nura
