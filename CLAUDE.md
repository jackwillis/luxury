# CLAUDE.md

Luxury is an educational Scheme interpreter in Haskell (GHC2024), built up from a bare-minimum REPL.

This is a learning project, but it is also developing a real point of view: correct Scheme semantics, a pleasant live programming experience, strong runtime behavior, and an implementation that uses Haskell's strengths deliberately.

Favor small, explicit changes over premature abstraction or optimization.

## Toolchain

GHC and cabal come from Homebrew, not ghcup.

`.vscode/settings.json` sets:

```json
"haskell.manageHLS": "PATH"
```

so the editor uses that toolchain.

## Commands

```bash
cabal build
cabal run luxury
cabal run scratch
cabal test
cabal test --test-options='--match "Reader.tokenize"'
```

- `luxury` is the REPL.
- `scratch` is for demos and manual experiments.
- The Hspec suite is the primary executable specification of current behavior.

## Layout

- `src/` is the internal `luxury-scheme` library used by the executables and tests.
- `luxury/Main.hs` contains the interactive REPL shell.
- `scratch/Main.hs` is a demo/scratch executable.
- `test/` contains one `*Spec.hs` per module and is wired together explicitly in `test/Main.hs`.

The current conceptual pipeline is:

```text
text
  ↓
Reader
  ↓
SExpr
  ↓
Eval
  ↓
Value
  ↓
Value.render
```

Number types live in `Number`. `SExpr` and `Value` import them directly rather than re-exporting them.

Modules use explicit export lists.

## Workflow

Specs are often committed before the implementation exists. Red tests are intentional and represent planned behavior.

Run `cabal test` before assuming a feature is complete.

Prefer implementing one coherent semantic step at a time rather than making a broad set of tests green by hard-coding special cases.

`readSExpr` reads one form and returns leftover tokens. This is not an error.

`readProgram` repeatedly calls it over the whole input.

`ReaderError` deliberately has no trailing-token error. A strict single-form reader would be a separate function.

## Design philosophy

Luxury should remain semantics-first.

Establish what Scheme constructs mean in the simple tree-walking evaluator before designing bytecode, compiler optimizations, or a VM.

The future VM should implement already-understood semantics rather than becoming a second language implementation with subtly different behavior.

Keep these layers conceptually distinct:

```text
Reader syntax
Runtime values
Evaluation semantics
Runtime state
Interactive tooling
```

In particular:

- `SExpr` represents reader-level syntax and data.
- `Value` represents runtime values.
- `Eval` decides what syntax means.
- Reader and renderer modules should not accumulate evaluation semantics.

Quotation is a good example of this distinction:

```text
SExpr.Symbol "foo"
```

is syntax naming `foo`, while:

```text
Value.Symbol "foo"
```

is the runtime symbol datum `foo`.

## Naming

Use names that are clear in context.

Conventional abbreviations are fine when they are standard and immediately recognizable:

```text
Eval
Env
SExpr
AST
VM
IR
REPL
```

Avoid shortening names merely for brevity.

Consistency matters more than maximizing either verbosity or terseness.

## Haskell as runtime leverage

Haskell is not just an implementation language for Luxury. Use the GHC runtime and type system as leverage.

Useful properties include:

- garbage collection
- arbitrary-precision integers
- exact rational support through Haskell libraries
- algebraic data types
- exhaustive pattern matching
- exceptions
- concurrency primitives
- laziness
- explicit strictness
- mature immutable data structures
- profiling and heap tooling
- portable native executables

Scheme semantics should remain explicit even when Haskell makes an implementation convenient.

Do not accidentally let Haskell's evaluation strategy redefine Scheme's evaluation strategy.

Normal Scheme procedure application should remain eager unless Scheme semantics explicitly say otherwise.

Use Haskell laziness intentionally for things such as promises, streams, compiler structures, or deferred tooling where it is semantically useful.

Prefer strict runtime/accounting state where predictable resource behavior matters.

## Type classes

Structural type classes such as:

```haskell
Eq
Show
```

are generally safe to derive when their meaning is obvious.

Treat behavioral type classes as semantic commitments rather than conveniences.

Examples include:

```haskell
Num
Ord
Functor
Semigroup
Monoid
```

Do not add them merely because an instance can be written.

Ask whether the laws and expected meaning of the class genuinely match the Luxury type.

For example, a `Num Number` instance affects how Scheme numeric semantics are represented and should wait until the numeric model is understood.

## Environments and procedures

Do not hard-code ordinary Scheme procedures into `Eval` by matching symbol spellings if they can instead live in an environment as runtime values.

For example, avoid making evaluation fundamentally work like:

```haskell
eval (List [Symbol "+", ...]) = ...
```

The intended direction is:

```text
look up "+"
evaluate operands
apply resulting procedure value
```

Special forms such as:

```scheme
quote
if
lambda
define
```

belong in evaluation semantics because they control whether or how their subexpressions are evaluated.

Ordinary procedures such as:

```scheme
+
-
*
/
cons
car
cdr
```

should eventually be ordinary runtime bindings.

## Performance

Luxury is currently a tree-walking interpreter. That is intentional.

Do not optimize prematurely.

Simple representations are acceptable when they preserve clean replacement points for later improvements.

Possible future changes include:

- interned symbols
- indexed lexical slots
- explicit stack frames
- bytecode
- a Haskell-based VM
- a managed guest heap
- more efficient vectors and strings

Keep semantics independent of those implementation choices.

The eventual architecture may look like:

```text
source
  ↓
Reader
  ↓
syntax / core forms
  ↓
compiler
  ↓
bytecode
  ↓
Luxury VM
```

but that should happen after the core evaluator has taught us what the VM needs.

## Resource awareness and hardening

Host safety and guest safety are different concerns.

Haskell gives Luxury strong protection against many classes of memory corruption and unsafe host-language bugs.

It does not prevent Scheme code from:

- looping forever
- allocating forever
- retaining large object graphs
- constructing enormous integers
- exhausting CPU or memory
- requesting dangerous host capabilities

Luxury should eventually make guest resource policy explicit.

Likely future mechanisms include:

- instruction or fuel limits
- allocation limits
- bounded reader input
- bounded nesting
- interruptible evaluation
- explicit VM stack limits
- capability-controlled I/O
- top-level exception containment

These are long-term design goals, not reasons to complicate today's evaluator.

## Memory discipline

Runaway memory use is a first-class concern.

Avoid designs that make future accounting impossible.

Guest allocation paths should be identifiable and ideally centralized enough that later they can support:

- allocation accounting
- quotas
- diagnostics
- guest-level heap policies

Be cautious about accidental retention through:

- closure environments
- promises
- provenance graphs
- ASTs retained after compilation
- lazy thunk chains
- old runtime states
- large debug metadata attached directly to values

Provenance and debug metadata should preferably use lightweight identifiers or side tables rather than recursively embedding entire histories into every value.

A future hardened VM may distinguish:

```text
Haskell RTS heap
Luxury guest heap
```

with Haskell managing the VM implementation and Luxury explicitly managing guest objects.

That is not required now, but current abstractions should not make it impossible later.

## Errors

Expected Scheme failures should be represented as structured runtime errors rather than uncaught host exceptions.

Prefer shapes such as:

```haskell
Either EvalError Value
```

or a later richer runtime result type.

Type errors, arity errors, unbound variables, malformed special forms, resource exhaustion, and permission failures should eventually be ordinary interpreter outcomes.

Reserve host exceptions for genuinely exceptional implementation failures and contain them at the outer runtime boundary where appropriate.

## Tail calls and execution model

Do not rely on the Haskell call stack as the permanent Scheme call stack.

Proper tail calls are part of Scheme semantics and will eventually require deliberate implementation support.

A future explicit evaluator loop or VM should own Scheme control flow.

This also improves:

- interruptibility
- predictable stack usage
- tracing
- debugging
- resource accounting

## Interactive experience

The REPL is a first-class part of Luxury, not a demo shell.

The desired experience is closer to:

- IRB
- Pry
- Lisp development environments
- Smalltalk-style live programming

than to a simple compile-run-debug loop.

Luxury should feel like a running system that can be explored and changed interactively.

Long-term REPL features may include:

```text
:help
:load
:reload
:bindings
:inspect
:source
:trace
:untrace
:history
```

as well as:

- multiline input
- persistent bindings
- procedure redefinition
- good pretty-printing
- useful error messages
- Ctrl-C interruption without killing the session
- runtime inspection
- source lookup
- tracing
- live experimentation

Think of Scheme code as something that can be developed inside an ongoing environment, not only as files executed from scratch.

## REPL state

The REPL environment should eventually be a real long-lived runtime object.

Avoid assuming that every input line evaluates against a fresh empty environment.

A future state may contain things such as:

```haskell
data ReplState = ReplState
  { environment :: Env
  , history     :: ...
  , loadedFiles :: ...
  , options     :: ...
  }
```

The exact representation should emerge from implementation needs rather than being designed prematurely.

## Source metadata

Source spans and structured metadata will likely become valuable across several subsystems:

- reader diagnostics
- evaluator errors
- stack traces
- procedure source lookup
- tracing
- provenance
- language-server features

When source locations are introduced, prefer a representation reusable across these concerns rather than separate ad hoc location systems.

## Editor tooling and language server

A Luxury language server is a long-term goal.

It should grow out of information the interpreter/compiler already understands rather than becoming a separate early project.

Source spans, structured diagnostics, name resolution, and environment metadata should eventually support features such as:

- diagnostics
- hover information
- go to definition
- find references
- document symbols
- completion
- inline documentation for built-ins

The most interesting tooling direction combines static editor information with a connected live Luxury runtime.

Possible future actions include:

```text
Luxury: Evaluate Selection
Luxury: Evaluate Definition
Luxury: Send to REPL
Luxury: Inspect Binding
Luxury: Trace Procedure
Luxury: Reload Definition
```

The editor could eventually understand both:

```text
where a name is defined in source
```

and:

```text
what that name is currently bound to in the live runtime
```

That live/static bridge is part of the same design goal as the REPL.

## Provenance

Optional provenance is a possible long-term Luxury feature, especially for debugging, testing, explanation, and live inspection.

It should remain optional and lightweight.

Do not make every runtime value carry an unbounded recursive history.

Prefer IDs, side tables, bounded records, or opt-in tracing mechanisms.

Provenance should improve observability without becoming a source of runaway memory use.

## Testing

Luxury should have an unusually good built-in testing story.

The current Hspec suite specifies the Haskell implementation, but the Scheme runtime may eventually grow first-class Scheme-side testing utilities.

Matcher objects, closures, structured failure information, source spans, and optional provenance could combine into a particularly good interactive testing experience.

Testing should remain explicit and understandable rather than magical.

## General rule

When choosing between two designs, prefer the one that:

1. expresses Scheme semantics clearly,
2. is easy to test,
3. preserves future implementation freedom,
4. makes failures explicit,
5. avoids unnecessary host-language cleverness,
6. supports a pleasant live programming experience,
7. leaves room for predictable resource control later.

Luxury should be simple now without becoming boxed in later.
