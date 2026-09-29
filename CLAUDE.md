# CLAUDE.md

An educational Scheme interpreter in Haskell (GHC2024), built up from a bare-minimum REPL. This is a learning project: favor small, explicit changes over abstraction or premature generalization.

## Toolchain

GHC and cabal come from Homebrew, not ghcup. `.vscode/settings.json` sets `"haskell.manageHLS": "PATH"` so the editor uses that toolchain.

## Commands

```bash
cabal build        # library and both executables
cabal run luxury   # REPL
cabal run scratch  # scratch/demo executable for manual checks
cabal test         # hspec suite
cabal test --test-options='--match "Reader.tokenize"'   # single example/group
```

## Layout

- `src/` is an internal library (`luxury-scheme`) used by both executables and the tests. Add new modules to `exposed-modules` in the cabal file.
- `luxury/Main.hs` is the REPL: read, eval, render.
- `scratch/Main.hs` is a demo executable, not part of the REPL.
- `test/` has one `*Spec.hs` per module, wired together by hand in `test/Main.hs` and listed in the test-suite's `other-modules`.

Pipeline: text → `Reader` → `SExpr` → `Eval` → `Value` → `Value.render`. Number types live in `Number`, which `SExpr` and `Value` import directly (no re-exports). Modules use explicit export lists.

## Workflow

Specs are often committed before the implementation exists (red), with the implementation in a later commit. Run `cabal test` to see what is still red before assuming something is complete.

`readSExpr` reads one form and returns leftover tokens (not an error); `readProgram` loops it over the whole input. `ReaderError` deliberately has no trailing-tokens case; a strict single-form reader would be a new function.
