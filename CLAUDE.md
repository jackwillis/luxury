# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

An educational Scheme interpreter written in Haskell (GHC2024), built up from a bare-minimum REPL. This is a learning project — favor small, explicit, well-commented-by-tests changes over abstraction or premature generalization.

## Toolchain

GHC and cabal are installed via **Homebrew**, not ghcup (`brew install ghc cabal-install`, plus `haskell-language-server` for editor support). The VS Code Haskell extension is configured via `.vscode/settings.json` with `"haskell.manageHLS": "PATH"` so it uses the Homebrew toolchain on `PATH` instead of trying to manage its own ghcup-based one.

## Commands

```bash
cabal build          # build the library and both executables
cabal run luxury      # run the REPL
cabal run scratch     # run the scratch/demo executable
cabal test            # run the hspec test suite
```

Run a single spec file or example with hspec's `--match`:

```bash
cabal test --test-options='--match "Reader.tokenize"'
```

## Architecture

Each component lives in its own top-level directory, and shared code is an internal `library` (not just `other-modules` duplicated across executables — that caused ambiguous-target errors in `cabal build <file>` when the same module was listed under multiple executables):

- `src/` — internal library (`exposed-modules: SExpr, Reader`), depended on by both executables and the test suite via `build-depends: luxury-scheme`.
  - `SExpr.hs` — the `SExpr` AST (`Symbol String | Number Number | Boolean Bool | List [SExpr]`, where `Number = ExactInteger Integer | InexactReal Double`) and `render :: SExpr -> String` (S-expression → text). Booleans read/render as `#t`/`#f` (case-insensitive on read: `#t`/`#T`/`#f`/`#F`); a `#`-prefixed atom that isn't one of those falls back to `Symbol`.
  - `Reader.hs` — the reader/parser pipeline: `tokenize :: String -> [Token]`, `readSExpr :: [Token] -> Either ReaderError (SExpr, [Token])` (reads **one** form, returns leftover tokens — it is not an error for tokens to remain), and `readProgram :: String -> Either ReaderError [SExpr]` (a program is a *sequence* of top-level forms, read by looping `readSExpr` until no tokens remain — not a single expression). Both are fully implemented, not stubs.
- `luxury/Main.hs` — the REPL executable: reads a line via `haskeline`, calls `readProgram`, renders each resulting form.
- `scratch/Main.hs` — a scratch/demo executable exercising `SExpr`/`Reader` manually; not part of the REPL, useful for quick manual checks of new library code.
- `test/` — hspec suite, one `*Spec.hs` per library module, wired together by hand in `test/Main.hs` (no `hspec-discover`).

Both library modules use explicit export lists (only the public API — the AST/token/error types with all constructors, and the top-level functions), hiding internal helpers like `isTokenDelimiter` and `renderTokens`.

### `ReaderError` has no `TrailingTokens` case

An earlier design had `ReaderError` carry a `TrailingTokens [Token]` case for "leftover input after reading one expression." It was removed: `readSExpr` returns leftovers as part of a normal success (not an error), and `readProgram` loops over `readSExpr` until no tokens remain, so it has no concept of "trailing" input either. If a stricter "read exactly one form and reject anything else" mode is ever wanted (e.g. for REPL-style single-expression-per-line input), that's a new function to design from scratch, not a dormant `ReaderError` case to revive.

### Test-driven workflow

Specs are routinely written and committed *before* the implementation exists (red), then the implementation follows in a separate commit. When touching `src/Reader.hs` or `src/SExpr.hs`, check `cabal test` output to see which specs are still red before assuming a function is complete.
