# Luxury Scheme Roadmap

This document tracks the broad implementation direction for Luxury.

It is intentionally lighter than an issue tracker.

The rough sequence is:

> **Become Scheme.**

> **Become a useful Scheme.**

> **Become Luxury.**

> **Become fast when measurement says it matters.**

## Now

The immediate goal is to turn the interpreter skeleton into a programmable Scheme.

### Procedure application

- apply primitive procedures
- evaluate arguments
- validate arity
- produce useful type/application errors

### Definitions and closures

- `define`
- `lambda`
- lexical environment frames
- closures
- recursive definitions

Early milestone:

```scheme
(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))
```

### Core control

- `if`
- `begin`
- `set!`
- `and`
- `or`
- `cond`
- `let`
- `let*`
- `letrec`

Use macro expansion for derived forms where appropriate.

### Core procedures

- arithmetic
- numeric comparisons
- pair/list operations
- equality predicates
- basic type predicates

## Next: core Scheme

### Data types

- dotted-pair reader syntax
- strings
- characters
- vectors
- broader numeric support

### Proper tail recursion

Move away from relying on the Haskell stack for Scheme tail calls.

Candidate implementations:

- trampoline
- explicit evaluation loop
- explicit continuation representation

### Exceptions

Implement:

- `raise`
- `with-exception-handler`
- `error`
- `guard`
- eventually `raise-continuable`

### Ports and I/O

Implement:

- current ports
- textual ports
- binary ports
- reading and writing
- file ports
- file helpers

### Process context

Implement:

- command-line arguments
- environment access
- exit behavior

### Macros

Implement hygienic macro facilities.

### Records

Implement R7RS record facilities.

### Source spans

Begin preserving:

- source/file
- line
- column
- span

Do this early enough that diagnostics do not require redesigning the reader later.

## R7RS-small milestone

Work toward broad R7RS-small compatibility.

R7RS-small remains the semantic north star even when experiments happen in parallel.

## Practical Scheme

After the core is healthy, selectively adopt useful Scheme ecosystem libraries.

Candidates include:

- richer list operations
- hash tables
- mappings
- sets and bags
- generators
- comparators
- vector utilities
- formatting
- useful numeric libraries

Prefer mature SRFIs and R7RS-large work before inventing parallel APIs.

### Generators and sequences

Adopt established Scheme generator conventions before designing a custom iterator model.

### Pattern matching

Study existing Scheme approaches.

Prefer a hygienic macro implementation.

## Luxury features

### Diagnostics

Build structured, source-aware errors.

Target output such as:

```text
car expected a pair

  (car 42)
       ^^

received:
  42 : exact integer
```

### Runtime inspection

Add support for inspecting:

- values
- records
- procedures
- environments
- source definitions
- eventual protocols

Possible REPL commands:

```text
,inspect
,type
,env
,help
```

### Protocols and generic operations

Before implementation, study:

- Common Lisp generic functions
- CLOS
- Clojure protocols
- Clojure multimethods
- Scheme precedents

Begin with the smallest useful dispatch system.

Possible surface:

```scheme
(define-protocol Matcher
  matches?
  describe
  explain-mismatch)
```

### Matcher-oriented testing

Build composable matcher values:

```scheme
(expect result
  (all-of
    (has-status 'open)
    (has-port 443)))
```

Support structured mismatch explanations.

### Property testing

Add:

- generators
- properties
- shrinking
- readable counterexamples

Reuse established Scheme generator abstractions where practical.

### Persistent collections

Evaluate persistent immutable:

- vectors
- maps
- sets

as a distinct facility from ordinary mutable Scheme collections.

### Structured concurrency

Explore:

```scheme
(with-task-group group
  (spawn group worker-a)
  (spawn group worker-b))
```

with:

- cancellation
- deadlines
- bounded concurrency
- failure propagation
- child-task lifetime

### Supervision

Explore Erlang/OTP-inspired supervision for long-running systems.

### Systems libraries

Provide enough runtime support for:

- TCP
- UDP
- DNS
- processes
- filesystem traversal
- clocks
- deadlines

Keep higher-level behavior in Scheme where practical.

## Real-program milestones

Use small programs as fitness tests.

Candidates:

```text
lcat
lwc
lgrep
lfind
lxargs
lsort
luniq
lnc
lscan
```

They exercise different parts of the language:

- `lcat` — ports and files
- `lwc` — streaming and text processing
- `lgrep` — strings and matching
- `lfind` — filesystem traversal
- `lxargs` — process execution
- `lnc` — sockets
- `lscan` — networking, deadlines, concurrency

These should be Luxury-native tools rather than strict compatibility clones.

## Developer environment

### REPL

Evolve `ilux` into a live workspace with:

- multiline editing
- history
- pretty printing
- source-aware diagnostics
- tracebacks
- value inspection
- environment inspection
- documentation lookup
- debugger support
- profiling

### Tooling

Explore:

- formatter
- language server
- debugger
- profiler
- test runner
- documentation browser
- editor integration

## Distribution

Make Luxury usable without installing Haskell.

Initial platform targets:

- Linux x86_64
- Linux aarch64
- macOS x86_64
- macOS arm64

Potential distribution formats:

- release archives
- Homebrew tap
- `.deb`
- `.rpm`

Keep separate:

```text
runtime implementation
standard library
third-party packages
user application
```

Do not build a package manager until there is a real ecosystem that needs one.

## Performance

Performance work should follow profiling.

Possible later work:

- compiler
- bytecode
- bytecode VM
- explicit stacks and frames
- compact runtime representations
- tagged/immediate scalar values
- mutable internal vectors
- specialized allocation
- profiling-guided optimization

The current interpreter can remain as a semantic reference implementation.

Luxury does not need to beat Chez Scheme to succeed.

Correct semantics, usable performance, strong tooling, and an enjoyable programming environment are already meaningful goals.

## Explicitly deferred

For now, defer:

- general provenance tracking
- a heavyweight class system
- full multimethods
- Common Lisp-style conditions/restarts beyond studying them as prior art
- custom reader syntax
- a package manager
- aggressive runtime optimization

These can be revisited when real programs demonstrate a need.

## Immediate implementation sequence

```text
primitive application
       ↓
define / lambda
       ↓
lexical environments + closures
       ↓
if / begin
       ↓
arithmetic + comparisons
       ↓
recursion
       ↓
proper Scheme semantics from there
```

Luxury does not need to solve its final architecture before becoming a useful little Scheme.