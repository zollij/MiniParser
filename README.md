# MiniParser

A minimalist parser combinator library written in Haskell. MiniParser operates
on `Data.Text` input and supports configurable comment handling for different
programming languages. The design was inspired by Graham Hutton's
[Programming in Haskell](https://people.cs.nott.ac.uk/pszgmh/pih.html) and
Heitor Toledo Lassarote de Paula's
["Parser Combinators in Haskell"](https://serokell.io/blog/parser-combinators-in-haskell).
I was also motivated by my relative inexperience with Haskell and the desire
to code something useful from scratch.

> **MHS support note.** Versions through **v0.4.2.0** supported both GHC and
> [MicroHs](https://github.com/augustss/MicroHs) (MHS). v0.5.0.0 and later
> are **GHC-only**. Empirical bisection during the v0.5 development cycle
> showed that mhs's compile-pass stack threshold is sensitive to the shape
> of every module in the library — making meaningful evolution of MiniParser
> require shape-preserving workarounds that cost more than the changes
> themselves. If you need MiniParser under MHS, pin to v0.4.2.0 (the tagged
> commit is the frozen MHS-supporting release).

## Table of Contents

- [Quick Start](#quick-start)
- [Building](#building)
- [Testing](#testing)
- [Project Structure](#project-structure)
- [Architecture: Circular Dependency Resolution](#architecture-circular-dependency-resolution)
- [Configuring Comment Parsers](#configuring-comment-parsers)
- [API Reference](#api-reference)
  - [Core Types](#core-types)
  - [Error Types](#error-types)
  - [Running Parsers](#running-parsers)
  - [Raw Parsers](#raw-parsers)
  - [Whitespace & Comment Stripping Parsers](#whitespace--comment-stripping-parsers)
  - [Numeric Parsers: Float vs Scientific](#numeric-parsers-float-vs-scientific)
  - [Floating-Point Parsers (token-aware)](#floating-point-parsers-token-aware)
  - [Scientific Parsers (token-aware)](#scientific-parsers-token-aware)
  - [Look-Ahead Parsers](#look-ahead-parsers)
  - [Take-Until & Take-While Parsers](#take-until--take-while-parsers)
  - [Combinator Parsers](#combinator-parsers)
  - [Standard Alternative Parsers](#standard-alternative-parsers)
  - [Whitespace and Comments](#whitespace-and-comments)
  - [Line-Oriented Parsers](#line-oriented-parsers)
  - [Expression Parser](#expression-parser)
  - [Utilities](#utilities)
- [Comment Module Reference](#comment-module-reference)
- [Writing a Custom Comment Module](#writing-a-custom-comment-module)
- [Performance Benchmarks](#performance-benchmarks)

## Quick Start

By default, MiniParser strips Haskell comments (`--` and `{- -}`) and
whitespace. If you are parsing a different language, replace
`src/MiniParser/Comments.hs` with one of the provided `Comments/*.hs` files.
For example, to parse C source code:

```bash
cp src/MiniParser/Comments/C.hs src/MiniParser/Comments.hs
# Then edit the module line to: module MiniParser.Comments (comments) where
```

See [Configuring Comment Parsers](#configuring-comment-parsers) for full
details and alternatives.

Use the library:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import MiniParser.Parser

main :: IO ()
main = do
  -- Parse a Haskell-style identifier (strips leading whitespace/comments)
  print $ parse identifierHaskell "  hello world"
  -- Right ("hello"," world")

  -- Parse a delimited list
  print $ parse (delimList ',' identifierHaskell) "foo, bar, baz"
  -- Right (["foo","bar","baz"],"")

  -- Parse a signed decimal number. `signed` strips leading whitespace
  -- and comments, then accepts an optional '-' or '+' that must be
  -- immediately followed by the digits (no space between sign and digits).
  print $ parse (signed decimal) "  -42 rest"
  -- Right (-42," rest")

  -- A space between the sign and the digits is rejected:
  print $ parse (signed decimal) "- 42"
  -- Left [...]

  -- The same rules apply to signed hexidecimal/octal/binary:
  print $ parse (signed hexidecimal) "  -0xff"
  -- Right (-255,"")

  -- Floating-point: `float` is strict-fractional (input must have a `.`
  -- or an `e/E` exponent). Compose with `signed` for an optional sign.
  print $ parse (signed float :: Parser Double) "  -3.14e2"
  -- Right (-314.0,"")

  -- Scientific: lenient (accepts integer-shape input) and lossless
  -- (preserves the exact literal as coefficient × 10^exponent).
  print $ parse (signed scientific) "  -3.14e2"
  -- Right (-314.0,"")
  print $ parse scientific "42"
  -- Right (42.0,"")    -- bare integer accepted; `float` would reject this
```

Note: The `OverloadedStrings` extension lets you write string literals that
are automatically converted to `Text`. Without it, you must use `Data.Text.pack`
to convert `String` values to `Text`.

## Building

A `Makefile` wraps the `cabal` commands.

```bash
make build         # cabal build all
make test          # build + run every test suite
make clean         # cabal clean
make performance   # run perf-test only
```

The `ghc-*` aliases (`ghc-build`, `ghc-test`, `ghc-clean`, `ghc-performance`)
are equivalent — they exist for symmetry with earlier MHS-supporting
releases that had a parallel `mhs-*` set.

## Testing

There are 9 test suites defined in `MiniParser.cabal`:

| Suite                    | File                   | Tests | Description |
|--------------------------|------------------------|-------|-------------|
| `MiniParser-test`        | `test/Test.hs` (+ `PosTests.hs`) | 313 | Core parser tests (HUnit + QuickCheck) |
| `expr-parser-test`       | `test/TestExprParser.hs` | 111 | Expression parser tests |
| `comments-c-test`        | `examples/TestC.hs`    | 21   | C comment parser tests |
| `comments-haskell-test`  | `examples/TestHaskell.hs` | 23 | Haskell comment parser tests |
| `comments-jack-test`     | `examples/TestJack.hs` | 20   | Jack comment parser tests |
| `comments-java-test`     | `examples/TestJava.hs` | 20   | Java comment parser tests |
| `comments-kotlin-test`   | `examples/TestKotlin.hs` | 27 | Kotlin comment parser tests |
| `float-test`             | `test/TestFloat.hs`    | 64  | Strict-fractional float parser tests (HUnit + QuickCheck), including DoS exponent cap |
| `scientific-test`        | `test/TestScientific.hs` | 67 | Lenient scientific-number parser tests (lossless coefficient/exponent preservation) |
| `perf-test`              | `test/TestPerf.hs`     | 20  | Performance and large-input tests |

Run all suites with `make test`.

The core test suite (`Test.hs`) uses HUnit for deterministic assertions and
QuickCheck for property-based testing of character parsers, takeUntil parsers,
number parsers, and more.

The comment test suites (`TestC.hs`, `TestHaskell.hs`, `TestJack.hs`,
`TestJava.hs`, `TestKotlin.hs`) are standalone programs that test each comment
module's individual parsers, combined `comments` function, and integration with
`token`/`identifier`/`symbol`.

## Project Structure

```
MiniParser/
├── MiniParser.cabal                  -- Build configuration
├── Makefile                          -- Wraps cabal targets
├── README.md                         -- This documentation
├── CHANGELOG.md                      -- Version history
├── LICENSE                           -- BSD-3-Clause license
├── .gitignore                        -- Git ignore rules
├── src/
│   └── MiniParser/
│       ├── Base.hs                   -- Parser type, primitives, raw numeric parsers (dec/hex/oct/bin/fp/sci)
│       ├── Comments.hs               -- Default comment parser (Haskell: --, {- -})
│       ├── ExprParser.hs             -- Expression parser (buildExprParser, Operator)
│       ├── Parser.hs                 -- Re-exports Base + comment-aware parsers (token, decimal, signed, float, scientific, ...)
│       └── Comments/
│           ├── C.hs                  -- C-style comments: //, /* */
│           ├── Haskell.hs            -- Haskell comments: --, {- -} (nested)
│           ├── Jack.hs               -- Jack comments: //, /* */, /** */
│           ├── Java.hs               -- Java comments: //, /* */, /** */
│           └── Kotlin.hs             -- Kotlin comments: //, /* */ (nested), /** */
├── test/
│   ├── Test.hs                       -- Main test suite (HUnit + QuickCheck)
│   ├── PosTests.hs                   -- Position-tracking tests (other-modules of MiniParser-test)
│   ├── TestExprParser.hs             -- Expression parser tests
│   ├── TestFloat.hs                  -- Strict-fractional float parser tests (HUnit + QuickCheck)
│   ├── TestScientific.hs             -- Lenient Scientific parser tests (HUnit)
│   ├── TestPerf.hs                   -- Performance and large-input tests
│   └── TestHelpers.hs                -- Shared test utilities (stripPos, test)
└── examples/
    ├── TestC.hs                      -- C comment test suite
    ├── TestHaskell.hs                -- Haskell comment test suite
    ├── TestJack.hs                   -- Jack comment test suite
    ├── TestJava.hs                   -- Java comment test suite
    └── TestKotlin.hs                 -- Kotlin comment test suite
```

### File Descriptions

**`src/MiniParser/Base.hs`** -- The foundation of the library. Contains the
`Parser` newtype, `Error` type, `Functor`/`Applicative`/`Monad`/`Alternative`
instances, and all low-level parser primitives (`item`, `satisfy`, `char`,
`string`, `digit`, `letter`, `digits`, `identHaskell`, etc.). Houses every
numeric parser that doesn't need comment-handling: `dec`, `hex`, `oct`,
`bin`, `digs`, plus `fp` (strict-fractional float parser, returns
`RealFloat`) and `sci` (lenient `Scientific` parser, lossless). Also
contains `pDiscard` (the engine for comment stripping), look-ahead parsers,
take-until parsers, `choice`, and utility functions. This module has no
dependency on any comment parser. The parse stream type is `Text` (from
`Data.Text`).

**`src/MiniParser/ExprParser.hs`** -- Expression parser module, inspired by
Parsec's `buildExpressionParser`. Exports the `Operator` type (with
constructors `InfixL`, `InfixR`, `InfixN`, `Prefix`, `Postfix`) and the
`buildExprParser` function, which builds a full expression parser from an
operator table and a term parser. The operator table specifies fixity,
associativity, and precedence. Only depends on `Base.hs`. The source file
contains a detailed running example that traces the parsing of
`"-3 + 4 * 2 ^ 2"` step by step through each precedence level.

**`src/MiniParser/Comments.hs`** -- The default comment parser. Exports a single
function `comments :: Parser ()` that strips Haskell comments (`--` and
`{- -}`) and whitespace. **This is the file users replace** to change the
comment handling for their project. See
[Configuring Comment Parsers](#configuring-comment-parsers).

**`src/MiniParser/Parser.hs`** -- The main user-facing module. Re-exports
everything from `Base` and adds the higher-level parsers that depend on
comment-handling: `token`, `identifierHaskell`, `decimal`, `hexidecimal`,
`octal`, `binary`, `signed`, `symbol`, `character`, `delimList`, `trim`,
`row`, `splitLines`, `splitLinesT`, `letters`, plus the comment-stripping
numeric wrappers `float` / `expFloat` (strict-fractional, `RealFloat`) and
`scientific` / `expScientific` (lenient, `Data.Scientific.Scientific`).
The raw `fp` and `sci` parsers live in `Base.hs` (no comment handling
needed) and are re-exported here, so callers see the full numeric API
in one module — matching Megaparsec's `Char.Lexer` and Attoparsec's
`Data.Attoparsec.Text` consolidation.

**`src/MiniParser/Comments/C.hs`** -- C-style comment parser. Handles
end-of-line comments (`// ...`) and inline block comments (`/* ... */`).

**`src/MiniParser/Comments/Haskell.hs`** -- Haskell comment parser. Handles
end-of-line comments (`-- ...`) and block comments (`{- ... -}`).

**`src/MiniParser/Comments/Jack.hs`** -- Jack language comment parser (from the
Nand2Tetris course). Handles end-of-line (`// ...`), inline (`/* ... */`),
and API/doc comments (`/** ... */`).

**`src/MiniParser/Comments/Java.hs`** -- Java comment parser. Handles
end-of-line (`// ...`), inline (`/* ... */`), and Javadoc comments
(`/** ... */`). Same syntax as Jack.

**`src/MiniParser/Comments/Kotlin.hs`** -- Kotlin comment parser. Handles
end-of-line (`// ...`), nested block comments (`/* ... */`), and KDoc comments
(`/** ... */`). Unlike C/Java, Kotlin block comments nest.

**`test/Test.hs`** -- Main test suite (HUnit + QuickCheck), 313 tests
combined with the position-tracking cases in `PosTests.hs`. Covers every
exported parser plus QuickCheck round-trip and structural properties for
character parsers, text parsers, and number parsers.

**`test/PosTests.hs`** -- HUnit position-tracking tests, included as an
`other-modules` of `MiniParser-test` rather than its own test-suite.
Originally split out from `Test.hs` to stay under MHS's compile-pass stack
threshold; now kept split for the same reason as Megaparsec/Parsec keep
test files small (compilation latency).

**`test/TestExprParser.hs`** -- Expression parser test suite with 111 HUnit
tests. Covers basic arithmetic, precedence, left/right/non-associativity,
prefix and postfix operators, parenthesised sub-expressions, complex
expressions, whitespace handling, minimal operator table configurations,
and intentional parse failures (trailing operators, unmatched parentheses,
chained non-associative operators, etc.).

**`test/TestFloat.hs`** -- Strict-fractional float parser test suite for
`float` / `expFloat` / `fp`. Covers the strict semantics (rejects bare
integer-shape input matching Megaparsec's `float`), composition with
`signed`, the exponent-length DoS guard (inputs like `1e1000000000`
reject in microseconds), and a QuickCheck round-trip property (`show`
then parse via `signed float` for any `Double` in `[-1e30, 1e30]`).

**`test/TestScientific.hs`** -- Lenient `Scientific` parser test suite
for `scientific` / `expScientific` / `sci`. Covers the lenient semantics
(accepts bare integer input as `Sci.scientific n 0`), lossless
coefficient/exponent preservation across the full Scientific range,
the `Sci.isInteger` distinction between `42` and `42.0`, the
`Sci.toBoundedInteger` overflow-check pattern, and the same DoS
exponent-cap discipline shared with the float parsers.

**`test/TestPerf.hs`** -- Performance and large-input tests. Generates
configurable-size inputs (100KB+ at default scale) and verifies both
correctness and timing bounds.

**`test/TestHelpers.hs`** -- Shared test utilities (`stripPos`, `getPos`,
`test`) used by all test suites.

**`examples/TestC.hs`**, **`TestHaskell.hs`**, **`TestJack.hs`**,
**`TestJava.hs`**, **`TestKotlin.hs`** -- Comment module test suites. Each
defines local `token`/`identifier`/`symbol` helpers that use the
language-specific `comments` function, then tests individual comment parsers,
the combined `comments` function, and integration scenarios.

## Architecture: Circular Dependency Resolution

The library is split into three modules (`Base`, `Comments`, `Parser`) to
resolve a circular dependency:

```
  Comments.hs needs the Parser type   -->  imports Base
  Parser.hs needs comments function   -->  imports Comments
  Base.hs needs neither               -->  imported by both
```

Without this split, `Comments.hs` would need to import `Parser.hs` (to get
the `Parser` type and `pDiscard`), and `Parser.hs` would need to import
`Comments.hs` (to use `comments` inside `token`, `identifierHaskell`, etc.)
-- a cycle that Haskell does not allow.

`Base.hs` breaks the cycle by holding everything that both modules need:

- **`Base.hs`** -- `Parser` type, instances, all primitives, `pDiscard`
- **`Comments.hs`** -- imports `Base`, exports `comments :: Parser ()`
- **`Parser.hs`** -- imports `Base` + `Comments`, exports `token`, `identifierHaskell`, etc.

Users import `MiniParser.Parser` which re-exports all of `Base`, so the split
is invisible from the outside.

`ExprParser.hs` depends only on `Base.hs` (it does not use `token`, `symbol`,
or any comment-aware parsers). Users pass in their own operator parsers
(typically built with `symbol` from `Parser.hs`) when constructing the
operator table.

## Configuring Comment Parsers

MiniParser uses a **file-swap** approach for configuring comments. The module
`src/MiniParser/Comments.hs` is a user-replaceable file that controls what
`token`, `identifierHaskell`, `symbol`, and other higher-level parsers
treat as comments.

### Default: Haskell Comments

The default `Comments.hs` strips Haskell comments (`--` and `{- -}`) and
whitespace:

```haskell
module MiniParser.Comments (comments) where
import MiniParser.Base
import Control.Monad (void)

comments :: Parser ()
comments = pDiscard [void eolComment, void blockComment]
```

### Choosing a Different Language Comment Style

To make MiniParser handle C-style comments instead, copy the contents of
`Comments/C.hs` into `Comments.hs`, changing the module name:

```bash
# Back up the default
cp src/MiniParser/Comments.hs src/MiniParser/Comments.hs.default

# Install C comments
cp src/MiniParser/Comments/C.hs src/MiniParser/Comments.hs
```

Then edit the first line of `src/MiniParser/Comments.hs` to read:

```haskell
module MiniParser.Comments (comments) where
```

After rebuilding, `token`, `identifierHaskell`, `symbol`, etc. will
automatically strip C-style comments (`//` and `/* */`) instead of Haskell
comments.

### Whitespace Only (No Comments)

To strip whitespace only with no comment handling:

```haskell
module MiniParser.Comments (comments) where
import MiniParser.Base (Parser, pDiscard)

comments :: Parser ()
comments = pDiscard []
```

### Using Comments Directly (Without Replacing the File)

If you don't want to replace `Comments.hs`, you can import a comment module
directly and build your own token parsers:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import MiniParser.Base
import Data.Text (Text)
import qualified MiniParser.Comments.Java as JV

myToken :: Parser a -> Parser a
myToken p = do
  JV.comments
  p

myIdentifier :: Parser Text
myIdentifier = myToken identHaskell
```

This is the approach used by the example test files in `examples/`.

## API Reference

All parsers return `Either [Error] (a, Pos, Text)` when run with `parse`.
A successful parse yields `Right (result, position, remainingInput)`.
A failed parse yields `Left [errors]`.

The parse stream is `Data.Text.Text`. Use `OverloadedStrings` or `T.pack` to
convert `String` literals to `Text`.

### Naming Convention

Most parsers use plain names (`item`, `satisfy`, `takeUntil`, `choice`, etc.).
A handful retain a `p` prefix to avoid clashing with names from commonly
imported modules:

| Parser | Reason for `p` prefix |
|--------|-----------------------|
| `pTakeWhile`, `pTakeWhile1` | `takeWhile` is exported by `Prelude` (and `Data.Text`) |
| `pDiscard` | `discard` is exported by `Test.QuickCheck` |
| `pFail`, `pFailStr` | `fail` is a method of `Monad` (exported by `Prelude`) |

### Core Types

| Type | Description |
|------|-------------|
| `Parser a` | `newtype P (PState -> Either [Error] (a, PState))` -- a parser that produces a value of type `a` |
| `PState` | `PState !Pos !Text` -- parser state: current position and remaining input |
| `Pos` | `Pos !Int !Int` -- position in source: line number and column number (both 1-based by default) |
| `Error` | Sum type: `EndOfInput`, `Unexpected String String`, `Unexpected' String`, `CustomError String`, `Empty`, `ExpectedEndOfFile Char` |

Note: `Error` constructors use `String` for error messages (display only).
The parse stream itself is `Text`.

### Error Types

Parsers report failures via `Left [Error]`. The `Error` type has six
constructors:

| Constructor | Fields | Meaning | Produced by |
|-------------|--------|---------|-------------|
| `EndOfInput` | *(none)* | Parser needed more input but the stream is empty | `item`, `satisfy`, `char`, `digit`, `lower`, `upper`, `letter`, `alphanum`, `dec`, `hex`, `oct`, `bin`, `lookAhead`, `lookAheadMulti`, `nestedBlockComment` |
| `Unexpected` | `expected::String, actual::String` | Parser expected one thing but found another | `string` (expected the target string, got a prefix of the input) |
| `Unexpected'` | `actual::String` | Parser found an unexpected character (no specific expectation) | `satisfy`, `char`, `digit`, `lower`, `upper`, `letter`, `alphanum`, `pTakeWhile1`, `eof` |
| `CustomError` | `message::String` | User-defined error message | `pFailStr` |
| `Empty` | *(none)* | Identity element for `Alternative`; indicates a branch produced no result | `empty` (the `Alternative` identity) |
| `ExpectedEndOfFile` | `found::Char` | `eof` expected end of input but found more data | `eof` |

When `<|>` is used, the current implementation pDiscards the left branch's
errors and tries the right branch. Only the final failing branch's errors
are reported. This keeps error lists short but means earlier alternatives'
failures are not accumulated.

Use `errorsToString :: [Error] -> String` to convert an error list to a
human-readable string for display.

### Running Parsers

| Function | Type | Description |
|----------|------|-------------|
| `parse` | `Parser a -> Text -> Either [Error] (a, Pos, Text)` | Run a parser on input |

### Raw Parsers

These operate directly on input without stripping whitespace or comments.

| Parser | Type | Description |
|--------|------|-------------|
| `item` | `Parser Char` | Consume one character (any) |
| `satisfy` | `(Char -> Bool) -> Parser Char` | Consume one character satisfying a predicate |
| `digit` | `Parser Char` | Consume one digit (`0`-`9`) |
| `lower` | `Parser Char` | Consume one lowercase letter |
| `upper` | `Parser Char` | Consume one uppercase letter |
| `letter` | `Parser Char` | Consume one letter (any case) |
| `alphanum` | `Parser Char` | Consume one alphanumeric character |
| `char` | `Char -> Parser Char` | Consume a specific character |
| `string` | `Text -> Parser Text` | Match an exact text string |
| `identHaskell` | `Parser Text` | Parse a Haskell-style identifier (lowercase letter start, alphanumeric continuation). |
| `dec` | `Num a => Parser a` | Parse a decimal (base 10) number |
| `hex` | `Num a => Parser a` | Parse hex (base 16) digits (no `0x` prefix); accepts `0`-`9`, `a`-`f`, `A`-`F` |
| `oct` | `Num a => Parser a` | Parse octal (base 8) digits (no `0o` prefix); accepts `0`-`7` |
| `bin` | `Num a => Parser a` | Parse binary (base 2) digits (no `0b` prefix); accepts `0`-`1` |
| `digs` | `Num a => (Char -> Bool) -> a -> Parser a` | Shared digit-folding primitive: take chars matching the predicate, fold into `a` using the given positional multiplier. Used by `dec`/`hex`/`oct`/`bin`. |
| `digits` | `Parser Text` | One or more digits as `Text` (efficient alternative to `many digit`) |
| `letters` | `Parser Text` | One or more letters as `Text` (efficient alternative to `many letter`). Defined in `Parser.hs`, re-exported. |
| `sci` | `Int -> Parser Scientific` | Raw scientific-number parser (does not strip whitespace). **Lenient** — accepts integer-shape input (`"42"` → `Sci.scientific 42 0`), `.digits` fractional input, and `e`/`E[+-]?digits` exponents in any combination. Returns a lossless `Data.Scientific.Scientific`. The `Int` argument caps the number of digits allowed in the exponent (DoS guard); exceeding the cap is a hard parse failure. Does not handle a leading sign — compose with `signed`. |
| `fp` | `RealFloat r => Int -> Parser r` | Raw floating-point parser (does not strip whitespace). **Strict-fractional** — input must contain `.<digits>` *or* an `e`/`E` exponent; bare integer-shape input fails. Matches Megaparsec's `Text.Megaparsec.Char.Lexer.float`. Result is narrowed from the underlying `Scientific` via `Sci.toRealFloat` (IEEE correctly-rounded). The `Int` argument caps the exponent length (DoS guard). Does not handle a leading sign — compose with `signed`. |

### Whitespace & Comment Stripping Parsers

These strip leading whitespace and comments (as configured in `Comments.hs`)
before parsing.

| Parser | Type | Description |
|--------|------|-------------|
| `token` | `Parser a -> Parser a` | Strip comments/whitespace, then run parser |
| `identifierHaskell` | `Parser Text` | `token identHaskell` -- a Haskell-style identifier with leading whitespace/comments stripped. |
| `decimal` | `Num a => Parser a` | `token dec` |
| `hexidecimal` | `Num a => Parser a` | Unsigned hex literal with `0x` / `0X` prefix (e.g. `0xff`) |
| `octal` | `Num a => Parser a` | Unsigned octal literal with `0o` / `0O` prefix (e.g. `0o17`) |
| `binary` | `Num a => Parser a` | Unsigned binary literal with `0b` / `0B` prefix (e.g. `0b1010`) |
| `signed` | `Num a => Parser a -> Parser a` | Wrap any numeric parser to accept an optional leading `-` (negate) or `+` (no-op) sign. Strips leading whitespace and comments before the sign; rejects whitespace between the sign and the digits. So `signed decimal "  -42"` succeeds, `signed decimal "- 42"` fails. The same rules apply to `signed hexidecimal`, `signed octal`, `signed binary`, and `signed float`. |
| `symbol` | `Text -> Parser Text` | `token (string xs)` |
| `character` | `Char -> Parser Char` | `token (char c)` |
| `delimList` | `Char -> Parser a -> Parser [a]` | Parse a delimited list (e.g., comma-separated) |

> **Note:** All numeric parsers (`dec`, `hex`, `oct`, `bin`, `digs`,
> `decimal`, `hexidecimal`, `octal`, `binary`) and the `signed` combinator
> are polymorphic over `Num`. Specialize at the use site with a type
> annotation when the surrounding context doesn't pin down the result type
> — e.g. `parse (signed decimal :: Parser Integer) s` to avoid `Int`
> overflow on large literals, or `parse (hexidecimal :: Parser Word64) s`
> for a specific machine-width type. GHC defaults ambiguous `Num`
> constraints to `Integer`, so an annotation is only needed when defaulting
> can't apply.

### Numeric Parsers: Float vs Scientific

MiniParser ships two parallel numeric parser families. Pick based on
*shape strictness* and *result type*:

|                        | Lenient (accepts `"42"`) | Strict (requires `.` or `e/E`) |
|------------------------|--------------------------|--------------------------------|
| **Result: `RealFloat r`** | — *(use `fmap Sci.toRealFloat scientific` if you need this combination)* | `float`, `expFloat`, `fp` |
| **Result: `Scientific`** | `scientific`, `expScientific`, `sci` | — *(use `signed scientific` and reject integer shape downstream if needed)* |

- **Use `float` / `expFloat` / `fp`** when the source language lexically
  distinguishes integer literals from float literals (`42` is *not* a
  float in C, Haskell, JSON, …) and you want a `Double`/`Float` result.
- **Use `scientific` / `expScientific` / `sci`** when you want any
  numeric form accepted and the *exact literal* preserved (no IEEE
  rounding, distinguish `42` from `42.0` via `Sci.isInteger`,
  range-check via `Sci.toBoundedInteger` before narrowing).

All six parsers share the same exponent-length cap discipline and the
same lenient remainder behaviour for ill-formed exponent prefixes
(`"3e"`, `"3eX"` leave the prefix in the remainder). The strict family
*additionally* rejects bare integer-shape input.

The raw parsers (`fp`, `sci`) live in `MiniParser.Base` and are
documented in [Raw Parsers](#raw-parsers); the comment-stripping
wrappers documented below live in `MiniParser.Parser`.

### Floating-Point Parsers (token-aware)

Strict-fractional. Comment-stripping wrappers around `fp`. The result
is constrained to `RealFloat` (narrows via `Sci.toRealFloat`,
IEEE correctly-rounded).

| Parser | Type | Description |
|--------|------|-------------|
| `float` | `RealFloat f => Parser f` | `expFloat 4` — strips whitespace/comments, parses a strict-fractional decimal float, with a 4-digit exponent cap that covers all of `Double` (~`1e308`). Bare integer-shape input fails. |
| `expFloat` | `RealFloat f => Int -> Parser f` | `token (fp n)` — strips whitespace/comments, then parses with a caller-supplied exponent-digit cap. Use when you need a cap different from `float`'s default of 4. |

> **Migration note (0.5.x):** the type was narrowed from `RealFrac` to
> `RealFloat` in 0.5.1.0 (the underlying narrowing function
> `Sci.toRealFloat` is `RealFloat`-constrained). Callers who need
> `Rational` should use `Sci.toRational <$> scientific` (or
> `Sci.toRational <$> sci n` for the raw form). Strict-fractional
> semantics were added in 0.5.2.0; for the prior lenient `fp`, switch
> to `signed scientific`.

### Scientific Parsers (token-aware)

Lenient. Comment-stripping wrappers around `sci`. The result is
`Data.Scientific.Scientific` — exact representation as
*coefficient × 10^exponent*, no rounding.

| Parser | Type | Description |
|--------|------|-------------|
| `scientific` | `Parser Scientific` | `expScientific 4` — strips whitespace/comments, parses any decimal numeric form (integer, fractional, or with `e`/`E` exponent), with a 4-digit exponent cap. Default for most callers. |
| `expScientific` | `Int -> Parser Scientific` | `token (sci n)` — strips whitespace/comments, then parses with a caller-supplied exponent-digit cap. Use this when you need a different cap than `scientific`'s default of 4. |

The `Scientific` result is useful for:
- **Lossless preservation** of the user's literal across overflow checks
  (`Sci.toBoundedInteger`, `Sci.toRealFloat`, etc.).
- **Distinguishing integer-shape from fractional-shape input**: `"42"`
  parses as `Sci.scientific 42 0` (`Sci.isInteger` returns `True`);
  `"42.0"` parses as `Sci.scientific 420 (-1)` (`isInteger` returns
  `False`). The strict-`fp` family loses this distinction by rejecting
  the former.
- **Compiler frontends** that need to emit precise diagnostic messages
  about numeric literals before deciding which target type to use.

### Look-Ahead Parsers

These peek at input without consuming it.

| Parser | Type | Description |
|--------|------|-------------|
| `lookAhead` | `Parser Char` | Peek at next character without consuming |
| `lookAheadMulti` | `Int -> Parser Text` | Peek at next `n` characters without consuming |

### Take-Until & Take-While Parsers

| Parser | Type | Description |
|--------|------|-------------|
| `takeUntil` | `Char -> Parser Text` | Read until character (don't consume it) |
| `takeUntil'` | `Char -> Parser Text` | Read until character (consume and discard it) |
| `takeUntilStr` | `Text -> Parser Text` | Read until text (don't consume it) |
| `takeUntilStr'` | `Text -> Parser Text` | Read until text (consume and discard it) |
| `takeUntilEOL` | `Parser Text` | Read until end of line (don't consume EOL) |
| `takeUntilEOL'` | `Parser Text` | Read until end of line (consume EOL) |
| `takeAll` | `Parser Text` | Read all remaining input |
| `pTakeWhile` | `(Char -> Bool) -> Parser Text` | Read while predicate holds |
| `pTakeWhile1` | `(Char -> Bool) -> Parser Text` | Read one or more characters while predicate holds (fails if none match) |

### Combinator Parsers

| Parser | Type | Description |
|--------|------|-------------|
| `choice` | `[Parser a] -> Parser a` | Try parsers in order, return first success |
| `pDiscard` | `[Parser ()] -> Parser ()` | Strip whitespace + run comment parsers in a loop |
| `eof` | `Parser ()` | Succeed only at end of input |
| `pFailStr` | `String -> Parser a` | Always fail with a custom error message |
| `pFail` | `[Error] -> Parser a` | Always fail with specific errors |
| `try` | `Parser a -> Parser a` | Identity (backtracking is always on); provided for compatibility |
| `sepBy` | `Parser a -> Parser sep -> Parser [a]` | Zero or more occurrences separated by `sep` |
| `sepBy1` | `Parser a -> Parser sep -> Parser [a]` | One or more occurrences separated by `sep` |
| `nestedBlockComment` | `Text -> Text -> Parser Text` | Parse a nested block comment given open/close delimiters |

### Standard Alternative Parsers

These are from Haskell's `Alternative` type class (`Control.Applicative`).
MiniParser's `<|>` always backtracks.

| Parser | Type | Description |
|--------|------|-------------|
| `p <\|> q` | `Parser a -> Parser a -> Parser a` | Try `p`, if it fails try `q` (with backtracking) |
| `many p` | `Parser a -> Parser [a]` | Zero or more |
| `some p` | `Parser a -> Parser [a]` | One or more |
| `optional p` | `Parser a -> Parser (Maybe a)` | Zero or one |
| `empty` | `Parser a` | Always fail |

### Whitespace and Comments

| Parser | Module | Type | Description |
|--------|--------|------|-------------|
| `space` | Base | `Parser ()` | Consume zero or more whitespace characters |
| `comments` | Comments | `Parser ()` | Consume whitespace + comments (default: Haskell `--` and `{- -}`; configurable) |

### Line-Oriented Parsers

Defined in `Parser.hs`. These use `comments` for stripping.

| Parser / Function | Type | Description |
|-------------------|------|-------------|
| `trim` | `Parser Text` | Parse one line, strip leading/trailing whitespace and comments |
| `row` | `Parser Text` | Parse one row (returns `empty` at EOF, otherwise calls `trim`) |
| `splitLines` | `Parser [Text]` | Parse all rows (`many row`) |
| `splitLinesT` | `Text -> [Text]` | Pure function: split text into trimmed lines |

### Expression Parser

Defined in `MiniParser.ExprParser`. Builds a full expression parser from an
operator table and a term parser, inspired by Parsec's
`buildExpressionParser`.

| Type / Function | Description |
|-----------------|-------------|
| `Operator a` | Operator definition: `InfixL`, `InfixR`, `InfixN` (binary), `Prefix`, `Postfix` (unary). Each wraps a `Parser` that matches the operator symbol and returns the semantic function. |
| `buildExprParser` | `[[Operator a]] -> Parser a -> Parser a` -- Build an expression parser from an operator table (list of rows, highest precedence first) and a term parser. |

**Example:** A simple arithmetic expression parser with precedence,
associativity, and parentheses:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import MiniParser.Parser
import MiniParser.ExprParser

opTable :: [[Operator Int]]
opTable =
  [ [ Prefix  (symbol "-" *> pure negate)          ]  -- highest precedence
  , [ InfixR  (symbol "^" *> pure (^))             ]
  , [ InfixL  (symbol "*" *> pure (*))
    , InfixL  (symbol "/" *> pure div)              ]
  , [ InfixL  (symbol "+" *> pure (+))
    , InfixL  (symbol "-" *> pure (-))              ]  -- lowest precedence
  ]

term :: Parser Int
term = parens <|> decimal
  where parens = character '(' *> expr <* character ')'

expr :: Parser Int
expr = buildExprParser opTable term
```

```
> parse expr "2 + 3 * 4"
Right (14, ...)          -- 2 + (3*4)

> parse expr "2 ^ 3 ^ 2"
Right (512, ...)         -- 2 ^ (3^2)   (right-associative)

> parse expr "(1 + 2) * 3"
Right (9, ...)           -- parens override precedence
```

Parentheses are handled by the term parser, not the operator table. When
the term parser sees `(`, it calls `expr` recursively, which re-enters
the full precedence tower.

### Utilities

| Function | Type | Description |
|----------|------|-------------|
| `errorsToString` | `[Error] -> String` | Convert error list to a human-readable string |

## Comment Module Reference

Each comment module exports a `comments :: Parser ()` function and individual
comment parsers:

### MiniParser.Comments.C

| Parser | Type | Syntax |
|--------|------|--------|
| `comments` | `Parser ()` | Strips whitespace + all C comments |
| `eolComment` | `Parser Text` | `// ...` until end of line |
| `inlineComment` | `Parser Text` | `/* ... */` |

### MiniParser.Comments.Haskell

| Parser | Type | Syntax |
|--------|------|--------|
| `comments` | `Parser ()` | Strips whitespace + all Haskell comments |
| `eolComment` | `Parser Text` | `-- ...` until end of line |
| `blockComment` | `Parser Text` | `{- ... -}` (nested) |

### MiniParser.Comments.Jack

| Parser | Type | Syntax |
|--------|------|--------|
| `comments` | `Parser ()` | Strips whitespace + all Jack comments |
| `eolComment` | `Parser Text` | `// ...` until end of line |
| `apiComment` | `Parser [Text]` | `/** ... */` -- returns cleaned lines |
| `inlineComment` | `Parser Text` | `/* ... */` (rejects `/**`) |

### MiniParser.Comments.Java

| Parser | Type | Syntax |
|--------|------|--------|
| `comments` | `Parser ()` | Strips whitespace + all Java comments |
| `eolComment` | `Parser Text` | `// ...` until end of line |
| `javadocComment` | `Parser [Text]` | `/** ... */` -- returns cleaned lines |
| `inlineComment` | `Parser Text` | `/* ... */` (rejects `/**`) |

### MiniParser.Comments.Kotlin

| Parser | Type | Syntax |
|--------|------|--------|
| `comments` | `Parser ()` | Strips whitespace + all Kotlin comments |
| `eolComment` | `Parser Text` | `// ...` until end of line |
| `kDocComment` | `Parser [Text]` | `/** ... */` -- returns cleaned lines |
| `blockComment` | `Parser Text` | `/* ... */` (nested, rejects `/**`) |

## Writing a Custom Comment Module

To add comment support for a new language:

1. Create `src/MiniParser/Comments/MyLang.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module MiniParser.Comments.MyLang where

import MiniParser.Base
import Data.Text (Text)
import Control.Monad (void)

comments :: Parser ()
comments = pDiscard [void lineComment, void blockComment]

lineComment :: Parser Text
lineComment = do
  _ <- string "#"          -- e.g., Python/Ruby style
  takeUntilEOL'

blockComment :: Parser Text
blockComment = do
  _ <- string "'''"        -- e.g., Python triple-quote
  takeUntilStr' "'''"
```

2. Add it to `exposed-modules` in `MiniParser.cabal`.

3. To make it the default for `token`/`identifierHaskell`/etc., copy it to
   `src/MiniParser/Comments.hs` and change the module name to
   `MiniParser.Comments`.

**Important:** When your language has overlapping comment delimiters (e.g.,
`/*` vs `/**`), list the longer delimiter first in `pDiscard`. The parsers
are tried in order, so `javadocComment` must come before `inlineComment`.

## Performance Benchmarks

MiniParser was benchmarked against two widely-used Haskell parsing libraries:

- **[Attoparsec](https://hackage.haskell.org/package/attoparsec)** -- a
  high-performance parsing library optimized for speed.
- **[Megaparsec](https://hackage.haskell.org/package/megaparsec)** -- an
  industrial-strength parser combinator library with rich error reporting.

The benchmarks use three workloads from the
[parsers-bench](https://github.com/mrkkrp/parsers-bench) project (originally
created to guide Megaparsec development), adapted for `Text` input so all
three libraries parse the same data type. Each workload was scaled to ~100
entries for stable timings.

### Workloads

- **CSV** (100 rows, ~5KB) -- Quoted and unquoted fields, escaped double
  quotes, comma-separated records. Exercises string matching and
  alternative-based field dispatch.

- **Log** (100 entries, ~4KB) -- Structured log lines in the format
  `YYYY-MM-DD HH:MM:SS IP.AD.DR.ES product`. Exercises numeric parsing
  (`dec`/`decimal`) and fixed-format sequential parsing.

- **JSON** (806 values, ~7KB) -- Nested objects, arrays, strings, integers,
  booleans, and null. Exercises recursive descent, whitespace skipping, and
  `lookAhead`-driven dispatch.

### Results

Measured with [Criterion](https://hackage.haskell.org/package/criterion) on
GHC 9.6.7, `-O2`, Linux x86_64. Times are the Criterion `mean` estimate.

#### CSV (100 rows)

| Parser | Mean | Relative |
|--------|------|----------|
| Attoparsec | 100 μs | 1.0x |
| **MiniParser** | **121 μs** | **1.2x** |
| Megaparsec | 262 μs | 2.6x |

#### Log (100 entries)

| Parser | Mean | Relative |
|--------|------|----------|
| Attoparsec | 33 μs | 1.0x |
| **MiniParser** | **100 μs** | **3.0x** |
| Megaparsec | 132 μs | 4.0x |

#### JSON (806 values)

| Parser | Mean | Relative |
|--------|------|----------|
| Attoparsec | 98 μs | 1.0x |
| **MiniParser** | **318 μs** | **3.2x** |
| Megaparsec | 373 μs | 3.8x |

### Analysis

MiniParser is consistently faster than Megaparsec across all three workloads.
On CSV parsing (primarily string-oriented), MiniParser is within 20% of
Attoparsec. On number-heavy workloads (log and JSON), Attoparsec's highly
optimized `decimal` parser gives it a larger advantage. MiniParser's `dec`
parser uses a simpler `foldl'`-based implementation, which leaves room for
future optimization.

### Reproducing

The benchmark code lives in `perf-compare/`, a self-contained Cabal project
that symlinks to MiniParser's `src/` directory.

```bash
cd perf-compare
cabal bench bench-csv    # CSV benchmark
cabal bench bench-log    # Log benchmark
cabal bench bench-json   # JSON benchmark
cabal bench              # all three
```

### Use of AI

MiniParser was hand-coded. MiniParser's tests were written by claude code.
I also used claude code to help me determine time and space complexity of
the parser functions. This was a huge help in optimizing a few of the parsers
from exponential "big O" complexity down to something reasonable. Claude
has been a really helpful coding assistant but did not write this library.
