# Revision history for MiniParser

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
