# Luxury Scheme Design

This document records the current design principles behind Luxury.

It is not a language specification. R7RS-small is the primary specification for the Scheme language implemented by Luxury.

The purpose of this document is to explain where Luxury intends to follow existing Scheme practice, where it may extend it, and how those decisions should fit together.

## Principles

Luxury follows three broad rules.

> **Keep the semantics small. Put the luxury in the runtime, libraries, distribution, and tools.**

> **Standard first. Ecosystem second. Invention third.**

> **Put a feature in the lowest layer that genuinely needs to know about it, but no lower.**

That means:

- prefer libraries over evaluator special cases
- prefer hygienic macros over new core syntax
- prefer R7RS interfaces over host-language abstractions
- prefer mature SRFIs and Scheme precedent over parallel Luxury APIs
- keep tooling concerns out of language semantics
- invent only where there is a genuine gap

## Scheme first

Luxury is intended to remain recognizably Scheme.

R7RS-small is the semantic foundation and primary compatibility target.

Portable Scheme should remain portable Scheme.

Luxury-specific facilities should generally be opt-in through libraries such as:

```scheme
(import (luxury protocol))
(import (luxury test))
(import (luxury inspect))
(import (luxury concurrency))
(import (luxury net))
```

The rough layering is:

```text
R7RS-small semantics
        ↓
standard Scheme libraries
        ↓
selected SRFIs / R7RS-large work
        ↓
Luxury libraries and extensions
        ↓
Haskell runtime capabilities
        ↓
tooling and developer environment
```

## Standards policy

When Luxury needs a capability, use this order:

```text
Is it in R7RS-small?
       │
   yes ├──→ implement the standard interface
       │
       no
       ▼
Is it in R7RS-large work or a mature SRFI?
       │
   yes ├──→ strongly prefer that design
       │
       no
       ▼
Is there an established Scheme precedent?
       │
   yes ├──→ learn from or adopt it
       │
       no
       ▼
Design a Luxury-specific extension
```

Luxury should not become distinctive by renaming things Scheme already provides.

## Architectural layers

### 1. Core Scheme semantics

This layer defines what Scheme programs mean.

It includes:

- procedure application
- lexical scope
- proper tail recursion
- `quote`
- `if`
- `lambda`
- `define`
- `set!`
- `begin`
- numbers
- pairs and lists
- records
- ports
- exceptions
- hygienic macros
- continuations
- promises
- parameters
- multiple values
- `dynamic-wind`

This layer should remain conservative.

### 2. Standard Scheme libraries

Practical facilities should reuse R7RS, mature SRFIs, and useful R7RS-large work where possible.

Likely examples include:

- file and port APIs
- process context
- richer list operations
- hash tables
- mappings
- sets
- generators
- comparators
- vector utilities
- formatting

Luxury does not need to implement all of R7RS-large.

The goal is a coherent, useful subset.

### 3. Luxury libraries and extensions

Luxury-specific facilities should fill genuine gaps.

Current areas of interest include:

- protocols and generic operations
- matcher-oriented testing
- property testing
- runtime inspection
- structured concurrency
- supervision
- persistent immutable collections where they provide distinct semantics

Most of these should be implemented as libraries or macros.

### 4. Runtime capabilities

Some facilities require host/runtime support.

The Haskell runtime may provide primitives for:

- filesystem access
- sockets
- DNS
- processes
- clocks and deadlines
- threads/tasks
- cancellation
- STM
- channels
- profiling
- runtime inspection support

Higher-level behavior should live in Scheme where practical.

### 5. Tooling

Many of Luxury's most important features do not belong in language semantics.

Examples include:

- REPL commands
- source-aware diagnostics
- tracebacks
- debugger support
- profiling
- documentation lookup
- language-server integration
- formatting
- test reporting
- package tooling

The language can remain small while the environment becomes rich.

## Influences

### Scheme

Scheme provides the semantic center:

- small orthogonal abstractions
- lexical closures
- proper tail recursion
- continuations
- hygienic macros
- dynamic typing
- ports

### Haskell

Haskell shapes the implementation:

- algebraic data types
- pattern matching
- explicit effects
- garbage collection
- arbitrary-precision integers
- laziness
- lightweight concurrency
- STM
- profiling and heap tooling

The implementation should remain high-level until profiling justifies lower-level representations.

### Common Lisp

Common Lisp influences the programming environment:

- live interactive development
- generic functions
- multiple dispatch
- conditions and restarts
- runtime introspection
- rich standardized facilities

Luxury should study CLOS before finalizing generic dispatch.

Common Lisp conditions and restarts are important prior art for future recovery facilities, but R7RS exception semantics come first.

### Clojure

Clojure influences thinking about:

- protocols
- multimethods
- persistent immutable collections
- explicit mutable identities
- data-oriented programming

Luxury should study Clojure protocols before designing its own.

### Other influences

- Racket — macros and language tooling
- Erlang/OTP — supervision and fault-tolerant concurrency
- Python — discoverability, diagnostics, introspection, practical libraries
- Raku — roles, multi-dispatch, grammars
- Smalltalk — live systems and runtime inspection
- Ruby — dynamic-language ergonomics and REPL culture
- Unix — ports, streams, composable tools
- SRFIs — practical Scheme standardization

## Values, records, and encapsulation

Luxury should preserve Scheme's dynamic type model.

Values have runtime types.

Variables do not require static types.

The preferred mechanisms are:

```text
encapsulation  → lexical closures
structure      → Scheme records
polymorphism   → protocols / generic operations
identity       → runtime identity where necessary
mutation       → Scheme mutation
inspection     → protocols + tooling
resources      → runtime-backed values
```

A class system is not currently needed.

Closures already provide strong lexical encapsulation.

Records provide structured nominal data.

Those mechanisms should be exhausted before heavier object machinery is introduced.

## Protocols and generic operations

Protocols are the strongest current candidate for a genuine Luxury language extension.

Conceptually:

```scheme
(define-protocol Matcher
  matches?
  describe
  explain-mismatch)
```

Operations should retain ordinary Scheme call syntax:

```scheme
(matches? matcher actual)
(describe matcher)
(close socket)
(render value)
```

A possible implementation is for protocols to group generic operations:

```scheme
(define-generic matches?)
(define-generic describe)
(define-generic explain-mismatch)
```

Before settling this design, study:

- Common Lisp generic functions
- CLOS
- Clojure protocols
- Clojure multimethods
- relevant Scheme precedents

Begin with the smallest useful dispatch mechanism.

Do not commit prematurely to:

- classes
- inheritance
- full multiple dispatch
- method combinations

## Exceptions

R7RS exceptions belong to the core language.

Luxury should implement:

```scheme
raise
raise-continuable
with-exception-handler
guard
error
```

Exception objects are ordinary Scheme values.

Keep these ideas separate:

```text
exception mechanism
    =
control transfer

exception object
    =
value describing the failure
```

Structured record values can represent particular error categories.

An early evaluator may model non-continuable failures with `Either` or `ExceptT`.

Correct continuable exception behavior should arrive alongside explicit continuation machinery.

## Source spans and diagnostics

Source location information should be preserved early.

Syntax should eventually retain:

- source/file
- line
- column
- span

This enables diagnostics such as:

```text
car expected a pair

  (car 42)
       ^^

received:
  42 : exact integer
```

Source spans should support:

- evaluator errors
- tracebacks
- test failures
- macro diagnostics
- the REPL
- debugger integration
- language-server features

Diagnostics are a high-priority Luxury feature.

## Runtime inspection

Dynamic languages benefit from discoverability.

Luxury should make it possible to ask questions such as:

```text
What type is this value?
Which protocols does it satisfy?
What fields does this record expose?
What procedure is this?
Where was it defined?
What does this matcher expect?
```

Possible REPL behavior:

```text
ilux:042> matcher
#<matcher equal-to>

ilux:043> ,inspect matcher
type: <equal-matcher>
protocols:
  Matcher
  Inspectable
fields:
  expected = (a b c)
```

Inspection should be designed into the runtime rather than bolted on afterward.

## Testing

Testing is one area where Luxury may become distinctive.

Matchers should be ordinary composable Scheme values.

For example:

```scheme
(expect result
  (all-of
    (has-status 'open)
    (has-port 443)))
```

A matcher may conceptually support:

```text
matches?
describe
explain-mismatch
```

The evaluator should not know what a matcher is.

The testing library should use ordinary language facilities plus the relevant generic behavior.

Property testing may later add abstractions such as:

```text
Generator
Shrinkable
```

Existing Scheme generator conventions should be reused where practical.

## Explanation, not global provenance

General provenance tracking is intentionally deprioritized.

Luxury should not attach derivation histories to every runtime value.

Instead, focus on local explanation:

- source spans
- structured errors
- matcher explanation trees
- runtime inspection
- rich test failures

For example:

```text
all-of failed
├─ has-status open       ✓
└─ has-port 443          ✗
   actual ports: 22 80
```

General tracing or provenance can be revisited later if concrete use cases justify it.

## Ports and resources

If Scheme already defines an abstraction, host implementation details should not leak through.

For example:

```scheme
(call-with-input-file "hello.txt"
  (lambda (port)
    ...))
```

Scheme code should see a Scheme port.

Haskell handles remain an implementation detail.

Garbage collection handles memory lifetime, but not necessarily the lifetime of:

- files
- sockets
- subprocesses
- transactions
- tasks
- deadlines

Before introducing new resource primitives, Luxury should explore what can be built from:

- ports
- `dynamic-wind`
- exceptions
- continuations
- parameters
- library conventions

## Collections

Pairs and lists remain fundamental Scheme values.

Luxury should first adopt established Scheme facilities for:

- vectors
- richer list operations
- hash tables
- mappings
- sets
- generators
- comparators

Persistent immutable vectors, maps, or sets may still be worthwhile as distinct Luxury facilities.

If added, they should offer genuinely different semantics rather than merely different names.

## Pattern matching

Pattern matching fits naturally into Scheme:

```scheme
(match value
  ((list 'ok result)
   result)

  ((list 'error reason)
   (handle reason)))
```

It should preferably be implemented using hygienic macros.

Existing Scheme approaches should be studied first.

## Concurrency

Concurrency should eventually be structured.

Rather than exposing only detached threads, Luxury should model ownership and lifetime:

```scheme
(with-task-group group
  (spawn group worker-a)
  (spawn group worker-b))
```

Important concepts include:

- task lifetime
- cancellation
- deadlines
- waiting
- failure propagation
- bounded concurrency

Erlang/OTP supervision is an important influence for longer-running systems.

Haskell's concurrency and STM provide a strong implementation substrate.

## Implementation strategy

Luxury begins as a tree-walking interpreter because that keeps semantics simple and visible.

The likely long-term direction is:

```text
Scheme source
     ↓
reader
     ↓
syntax
     ↓
macro expansion
     ↓
core representation
     ↓
compiler
     ↓
bytecode
     ↓
Luxury VM
```

The tree-walking evaluator may remain as a semantic reference implementation.

Optimization should follow profiling.

## Non-goals for now

Luxury should resist:

- novelty for its own sake
- unnecessary reader syntax
- duplicating established Scheme APIs
- a heavyweight class system
- pervasive provenance
- a package manager before packages exist
- premature runtime optimization
- host-language abstractions leaking into Scheme semantics