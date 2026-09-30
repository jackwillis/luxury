# Luxury Scheme

Luxury is a little Scheme interpreter written in Haskell, built for fun and to
learn how languages work. The name is a nod to the eventual goal: a small language
with a comfortable REPL and useful tools around it.

## Where it is now

It's an early tree-walking interpreter. It has a reader, runtime values,
environment lookup, and evaluation of numbers, booleans, and quoted data.
Procedure calls, definitions, and closures are still on the to-do list.

R7RS-small is the target, but Luxury isn't a complete Scheme implementation yet.

## Things I'd like to try

- More of Scheme, starting with procedures, closures, and core control forms
- Useful libraries from the Scheme and SRFI ecosystem
- Clear errors and ways to inspect values and running code
- A REPL that makes it easy to poke around and experiment
- Scheme-side testing tools with helpful failure messages
- Eventually, perhaps a compiler and bytecode VM

Scheme sets the language direction, and Haskell shapes the implementation.
The interactive side takes inspiration from Lisp environments and Ruby's
IRB/Pry: exploring values, looking up source, and trying things in a running
session. Those are ideas for where the REPL could go, not features it has today.

The longer notes are in [DESIGN.md](DESIGN.md) and [ROADMAP.md](ROADMAP.md).
They're a collection of ideas, not a promise to build everything.

## Build and run

You'll need GHC with GHC2024 support (9.10 or newer) and Cabal.

```bash
cabal build
cabal test
cabal run ilux
```

`ilux` is the interactive interpreter. Try a quoted list:

```scheme
'(hello scheme)
```

`luxury` reads a file or standard input and prints the results:

```bash
cabal run luxury -- example.scm
printf "'(hello scheme)\n" | cabal run luxury
```

Some tests describe behavior that hasn't been implemented yet, so the suite may
have failures while the evaluator is being built out.

## License

[MIT](LICENSE). Feel free to use it, change it, or learn from it.
