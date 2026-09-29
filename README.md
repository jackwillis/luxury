# Luxury Scheme

Luxury is an experimental Scheme interpreter written in Haskell.

It is currently a small educational tree-walking interpreter, built incrementally toward R7RS semantics. The longer-term idea is more ambitious: **a small Scheme with an unusually capable runtime and excellent developer experience.**

Today, Luxury has a reader, runtime values, a REPL, and early evaluation support including `quote`. Much of the test suite intentionally describes behavior that has not been implemented yet.

## What would make it “Luxury”?

Aspirationally, Luxury should combine Scheme’s small core with some of the best affordances from Haskell, Ruby, Smalltalk, and practical Lisps:

- a Pry/IRB-like live REPL
- excellent errors and source diagnostics
- built-in testing, property testing, benchmarking, and profiling
- strong lazy programming support without making ordinary Scheme evaluation lazy
- lightweight concurrency, cancellation, STM, and runtime supervision
- a language server that can connect to the live runtime
- resource-aware script execution
- optional tracing and provenance
- curated batteries for things like SQLite, JSON, HTTP, and document processing
- a future bytecode VM implemented safely in Haskell

The goal is not a large language core. It is a **small language with a capable runtime, rich standard environment, and unusually good affordances**.

## Why Haskell?

Haskell is part of the design, not just the implementation language.

Luxury aims to take advantage of:

- garbage collection
- arbitrary-precision numbers
- laziness and call-by-need
- lightweight threads
- STM and structured concurrency primitives
- strong algebraic data types
- profiling and heap tooling
- a mature library ecosystem

Scheme semantics still come first. Haskell’s strengths should make Luxury nicer to use without quietly changing what Scheme means.

## Development

```bash
cabal build
cabal test
cabal run luxury
```

Luxury is a learning project first, so semantics are established carefully in the simple interpreter before more advanced runtime machinery is added.
