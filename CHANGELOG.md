# Revision history for MiniParser

## 0.6.0.0 -- 2026-05-01

* **Breaking — `MiniParser.Base` now has an explicit export list.** Three
  helpers that were previously *implicitly* exported (because the module
  was declared as `module MiniParser.Base where`) are no longer reachable
  from outside the module: `sciParts`, `buildScientific`, and
  `digitsToInteger`. Each was already marked `-- Internal:` in source
  comments, but the documentation didn't match the actual exported
  surface — Haskell exports every top-level binding when no export list
  is given. The new export list makes the internal/external boundary
  load-bearing rather than aspirational. Callers who were depending on
  any of these three names will need to inline equivalent logic;
  they were single-call-site helpers shared between `sci` and `fp`,
  not general-purpose utilities.
* Makefile bug fix: `make test` now runs the `scientific-test` test
  suite. The 0.5.1.0 cabal stanza was added correctly but never wired
  into the `ghc-test` target, so 67 tests were silently skipped on
  every `make test` invocation since 0.5.1.0.
* README synced with the current API surface. The Scientific parsers
  family (`scientific`, `expScientific`, `sci`) added in 0.5.1.0 was
  not documented; that's now corrected. The Floating-Point Parsers
  section reflects the 0.5.1.0 `RealFrac` → `RealFloat` narrowing and
  the 0.5.2.0 strict-fractional semantics. A new "Numeric Parsers:
  Float vs Scientific" overview section presents the two families as
  a 2×2 decision matrix (lenient/strict × `RealFloat`/`Scientific`),
  intended to make the choice obvious for new callers.

## 0.5.2.0 -- 2026-05-01

* **Breaking — `fp`, `float`, and `expFloat` are now strict-fractional.**
  The input must contain a `.` followed by digits, or an `e`/`E` (optionally
  signed) followed by digits. Bare integer-shape input (e.g. `42`) now
  fails. This matches the semantics of Megaparsec's
  `Text.Megaparsec.Char.Lexer.float`. Source languages that lexically
  distinguish integer literals from float literals get the right behaviour
  by default; callers that want lenient parsing should switch to
  `scientific`/`expScientific` (which remain lenient and accept
  integer-shape input as @Sci.scientific n 0@).
* The shared digit-shape parsing logic is factored into a private
  `sciParts` helper in `MiniParser.Base`. Both `sci` (lenient) and `fp`
  (strict) build on it, so the exponent-length cap discipline and the
  remainder-on-trailing-junk behaviour stay in lockstep across the two.
* The strictness check happens after `sciParts` succeeds, so inputs like
  `3..5`, `3.`, `3e`, `3e+`, `3eX` — which under the old lenient `fp`
  parsed as `3.0` with the partial component left in the remainder — now
  reject the whole input. Callers depending on the prior lenient
  partial-consume behaviour should switch to `signed scientific`
  (which preserves the prior consume-then-leave-remainder pattern).
* `MiniParser.Parser`'s `expFloat` now composes `token . fp` directly
  instead of `fmap Sci.toRealFloat . expScientific`. This is a
  consequence of `fp` being strict while `expScientific` stays lenient —
  the two are no longer equivalent up to type narrowing.

## 0.5.1.0 -- 2026-04-30

* Added scientific-number parsers: `sci :: Int -> Parser Scientific` (raw,
  exported from `MiniParser.Base`), `scientific :: Parser Scientific` and
  `expScientific :: Int -> Parser Scientific` (comment-eating, exported from
  `MiniParser.Parser`). They produce a `Data.Scientific.Scientific` value
  (coefficient × 10^exponent, exact representation), preserving the input
  literal losslessly. Useful for compiler frontends and any caller that
  wants to (a) round-trip a numeric literal across overflow checks,
  (b) distinguish "@42@" from "@42.0@" via `Sci.isInteger`, or
  (c) range-check via `Sci.toBoundedInteger` before narrowing to a
  specific numeric type. Same exponent-length cap discipline as the
  float parsers: default cap is 4 digits via `scientific`, overrideable
  via `expScientific n`.
* New dependency: the `scientific` package (>=0.3 && <0.4). The package
  is small (~600 LOC, no transitive baggage beyond `text` and `binary`).
* `fp`, `float`, and `expFloat` are now thin wrappers over the scientific
  primitives. They parse to `Scientific` and narrow via `Sci.toRealFloat`,
  which performs IEEE-correctly-rounded conversion. Same observable
  behaviour for all in-range `Double`/`Float` inputs; the existing 62-test
  float suite passes unchanged. The change replaces the previous
  `Numeric.readFloat`-based path, eliminating an unbounded `Rational`
  intermediate and shortening the conversion chain.
* **Breaking** — `fp`, `float`, `expFloat` type signatures narrowed from
  `RealFrac r =>` to `RealFloat r =>`. `Sci.toRealFloat` is RealFloat-
  constrained, so the polymorphism over Rational has been dropped.
  Callers wanting `Rational` should switch to `Sci.toRational <$> sci n`
  (raw) or `Sci.toRational <$> scientific` (comment-eating). In practice
  Rational targets are rare in float-parsing code; Double and Float
  remain unaffected.

## 0.5.0.0 -- 2026-04-30

* **Breaking — MHS (MicroHs) support is dropped.** v0.5.0.0 and later are
  GHC-only. The last MHS-supporting release is **v0.4.2.0**, tagged at
  the corresponding commit; pin to it with `MiniParser ^>= 0.4.2` if you
  need MHS. The reason: empirical bisection in this release cycle showed
  that mhs's `toplevel` compile-pass stack threshold on `test/Test.hs` is
  sensitive to the *shape of every module in the library*, not just
  modules `test/Test.hs` transitively imports. Removing a module from
  `exposed-modules`, replacing a module's contents with a thinner stub,
  or even renaming a top-level binding can tip mhs over — so meaningful
  evolution of MiniParser would require shape-preserving workarounds
  that cost more than the changes themselves. Freezing MHS support at
  v0.4.2.0 is honest about that constraint and lets the library evolve
  on its own terms.
* **Breaking — renamed `identifier` → `identifierHaskell` and `ident` →
  `identHaskell`** to make their Haskell-specific lexical rules explicit
  in the name. Both parsers require a lowercase-letter start followed by
  alphanumerics — rules that don't apply to most other languages (which
  permit leading underscores, dollar signs, Unicode letters, uppercase
  starts, etc.). The unqualified `identifier`/`ident` names suggested a
  generality the parsers don't have, and made it easy to silently produce
  wrong results when reaching for them while parsing other languages.
  Migration is a mechanical find-and-replace: `ident` → `identHaskell`,
  `identifier` → `identifierHaskell`.
* **Breaking — removed `identWith`.** It was deprecated in 0.4.x as "just
  the Haskell ident with special characters allowed; isn't very useful."
  Removed as part of this cycle's deprecation cleanup. Callers that need
  identifier parsing with non-Haskell rules should write a small custom
  parser using `pTakeWhile1` and the predicate of their choice.
* **`MiniParser.Float` module removed; the floating-point parsers (`float`,
  `expFloat`, `fp`) are now exported from `MiniParser.Parser`.** This
  consolidates all numeric parsers in one module — matching Megaparsec's
  `Text.Megaparsec.Char.Lexer` and Attoparsec's `Data.Attoparsec.Text`.
  Callers using `import MiniParser.Float` should switch to
  `import MiniParser.Parser` (or `import MiniParser.Parser
  (float, expFloat, fp)` for an explicit list).
* Removed CPP gating, `if !impl(ghc)` cabal conditionals, and the
  Makefile's `mhs-build`/`mhs-test`/`mhs-clean`/`mhs-performance` targets
  that existed to keep the prior MHS scaffolding alive. The Makefile is
  now GHC-only; `make build`, `make test`, `make clean` correspond
  one-to-one to their `ghc-*` counterparts.
* README rewritten to drop MHS-specific dependency lists, build
  instructions, compatibility notes, and the GHC-vs-MHS performance
  benchmark section. A short note in the intro points MHS users at
  v0.4.2.0.

## 0.4.2.0 -- 2026-04-28

* Added floating-point parsers `float`, `expFloat`, and `fp` in a new
  module `MiniParser.Float`. They are polymorphic over `RealFrac` so the
  result can be specialized to `Double`, `Float`, or `Rational` at the
  call site. They accept a leading digit run, an optional `.digits`
  fractional part, and an optional `e`/`E` exponent with optional `+`/`-`
  sign. Sign and leading whitespace are not handled by the raw `fp` —
  compose with `signed` and/or use `float`/`expFloat` (which call `token`),
  the same pattern as the integer parsers.
* Note that normally, I would not have put these new parsers in their
  own source file. I tried adding them to Parser.hs but when compiling
  using MHS, the tests hit some hard limits of MHS. Putting the parsers
  into Float.hs seems to have enabled the tests. Given the difficulty
  of adding new features to MiniParser, I may be forced to fork an MHS
  specific version. I need to investigate more. I can refactor this code
  after than investigation.
* The new parsers live in their own module rather than being added to
  `MiniParser.Parser`. `test/Test.hs` is right at mhs's `toplevel`-pass
  stack limit (the same limit that prompted the `PosTests` split in
  `0.4.1.0`), and adding any top-level definition to `MiniParser.Parser`
  pushes mhs's compile of `test/Test.hs` over into a stack overflow.
  Putting the new code in a module that `test/Test.hs` doesn't import
  keeps that file's elaboration size unchanged. Users opt in with
  `import MiniParser.Float`. Most users will use the `float` function,
  perhaps with the `signed` combinator: `signed float`.
    * `fp :: RealFrac r => Int -> Parser r` is the raw parser. The `Int`
      argument caps the number of digits allowed in the exponent portion
      of the input, bounding the intermediate `Integer` constructed inside
      `readFloat`. Exceeding the cap is a hard parse failure (not a silent
      backtrack), so e.g. `"1e1000000000"` is rejected rather than parsing
      as `1.0` with the rest left in the remainder.
    * `expFloat :: RealFrac f => Int -> Parser f` is `token (fp n)` —
      strips leading whitespace and comments, then parses with the
      caller-supplied exponent length cap.
    * `float :: RealFrac f => Parser f` is `expFloat 4`. Four exponent
      digits cover the full `Double` range (~`1e308`) with headroom; the
      cap also bounds the worst-case allocation inside `readFloat` to
      ~4 KB. Without a cap, an 11-byte input like `"1e999999999"` could
      drive `readFloat` to allocate a ~400 MB `Integer` and burn 10s+ of
      CPU per parse.
* Implementation goes through `Numeric.readFloat` after assembling the
  matched lexeme as `Text`. The exponent length check is lifted outside
  the `<|>` that makes the exponent optional, so `"3e"`, `"3eX"`, `"3e+"`
  and similar still parse leniently as `3.0` with the rest as remainder
  (the exponent prefix simply backtracks), while a too-long exponent
  fails the whole input cleanly.

## 0.4.1.0 -- 2026-04-26

* `signed` behavior refined to match the desired semantics uniformly across
  `signed decimal`, `signed hexidecimal`, `signed octal`, and `signed binary`:
    * Strips leading whitespace and comments before the sign, so
      `signed decimal "  -42"` succeeds and yields `-42`.
    * Requires the digits/prefix to immediately follow the sign — a
      lookahead after each sign character rejects intervening whitespace,
      so `signed decimal "- 42"` and `signed hexidecimal "  - 0xff"` fail.
    * Falls through to the wrapped parser unchanged when no sign is
      present, so `signed decimal "42"` and `signed decimal "  42"` both
      yield `42`.
  Implementation rewritten as `comments *> (negative <|> positive <|> p)`
  with `negative`/`positive` defined in a `where` clause. The previous
  do-block form was sensitive to indentation and could silently fall back
  to only the negative branch if the `<|>` operator landed at the same
  column as the do-block items.
* Tests added for the new `signed` semantics across all four bases (leading
  whitespace acceptance, mid-sign whitespace rejection), with the
  reference values: `signed hexidecimal "-0xfed1"`, `signed octal "+0o7541647"`,
  `signed binary "-0b10001010001"`, etc. QuickCheck round-trips for
  `signed octal`/`binary` (negative and explicit-`+` variants) added too.
* Position-tracking HUnit tests extracted to a separate `PosTests` module.
  This is a build-system fix: mhs's `toplevel` compile pass overflows on a
  single module with ~280+ list items, and splitting across modules keeps
  each below the threshold. GHC sees no functional difference.
* Test runner rewritten to a tasty-like colored, hierarchical format with
  no new dependencies. `TestHelpers` now exports `runHUnit`, `runQC`,
  `printResult`, `printResultLine`, `suiteHeader`, `sectionHeader`,
  `summaryLine`, and ANSI color helpers (`green`, `red`, `yellow`, `bold`).
  HUnit tests print right-aligned green `OK` / red `FAIL`; QuickCheck
  properties run silently and report `OK (N tests)` / yellow `GAVE UP` /
  `FAIL`; the perf-test format is unified with the rest. `NO_COLOR` and
  non-TTY stdout disable color automatically.
* Cabal: every test-suite stanza now lists `HUnit`, `QuickCheck`, and `time`
  in `build-depends` because the shared `TestHelpers` module imports all
  three. Library deps unchanged (`base`, `text`).

## 0.4.0.0 -- 2026-04-22

* **Breaking:** removed `int` (raw) and `integer` (token-level). Callers
  that need a signed number now compose explicitly with the new `signed`
  combinator: `signed decimal` replaces `integer`, and `signed dec`
  replaces `int`. The same combinator works with any other `Num a`
  parser: `signed hexidecimal`, `signed octal`, `signed binary`.
* Added `signed :: Num a => Parser a -> Parser a`. Behavior:
    * Strips leading whitespace and comments (via `comments`) before the
      sign, so `signed decimal "  -42"` succeeds and yields `-42`.
    * Accepts an optional `-` (negate) or `+` (no-op) sign immediately
      followed by the wrapped parser. A no-space lookahead after the sign
      rejects whitespace between the sign and the digits, so
      `signed decimal "- 42"` fails.
    * Falls through to the wrapped parser unchanged if no sign is
      present, so `signed decimal "42"` and `signed decimal "  42"` both
      succeed and yield `42`.
  The same rules apply uniformly to `signed octal`, `signed hexidecimal`,
  and `signed binary`: leading whitespace before the sign is OK, but no
  whitespace is permitted between the sign and the digits/prefix.
* Test suite extended with HUnit cases covering `signed decimal`,
  `signed hexidecimal`/`octal`/`binary`, leading-whitespace acceptance and
  mid-space rejection across all four bases, `signed` over a prefix
  cascade, and several adversarial inputs (`--42`, `+-42`, sign-then-EOF).
  Added QuickCheck round-trips for `signed decimal` (negative and
  explicit-`+` variants) and `signed hexidecimal`/`octal`/`binary`
  (negative and explicit-`+`).

## 0.3.0.0 -- 2026-04-22

* **Breaking:** numeric parsers are now polymorphic over `Num`. `dec`, `int`,
  `hex`, `oct`, `bin`, `digs`, `decimal`, `integer`, `hexidecimal`, `octal`,
  and `binary` now have signatures like `Num a => Parser a` instead of being
  hardcoded to `Parser Int`. Callers can specialize to `Integer` to avoid
  overflow on large literals, or to `Int`/`Word`/etc. as needed. A type
  annotation may be required if the surrounding context is ambiguous:
  `parse (decimal :: Parser Integer) "123"`.
* `digs` signature generalized to `Num a => (Char -> Bool) -> a -> Parser a`;
  positional multiplier and result share the same numeric type.
* Added an `Integer` round-trip QuickCheck property that exercises values
  larger than `Int` can hold.

## 0.2.0.0 -- 2026-04-22

* **Breaking:** renamed `nat` to `dec` and `natural` to `decimal` to make base
  explicit now that other bases are supported.
* Added `hex`, `oct`, `bin` raw parsers for base-16, base-8, and base-2 digit
  sequences, plus `digs :: (Char -> Bool) -> Int -> Parser Int`, a shared
  digit-folding helper used by `dec`/`hex`/`oct`/`bin`.
* Added `hexidecimal`, `octal`, `binary` token-level parsers that strip
  leading whitespace/comments and require a `0x`/`0X`, `0o`/`0O`, or
  `0b`/`0B` prefix. Unlike `integer`, these do not accept a leading `-`;
  signedness belongs in an expression layer.
* Test suite expanded with HUnit cases and QuickCheck round-trip properties
  for the new parsers, including backtracking behavior for prefix cascades
  like `binary <|> octal <|> hexidecimal <|> decimal`.

## 0.1.0.0 -- 2026-03-17

* First version. Released on an unsuspecting world.
