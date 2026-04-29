# Revision history for MiniParser

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
