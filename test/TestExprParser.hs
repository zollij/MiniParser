{-# LANGUAGE OverloadedStrings #-}
-- `decimal` is polymorphic over Num; a handful of ExprParser tests rely on
-- GHC defaulting the result to Integer. Silence -Wtype-defaults so tests
-- stay readable.
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import MiniParser.Base
import MiniParser.Parser
import MiniParser.ExprParser
import TestHelpers (stripPos)
import Test.HUnit
import Control.Applicative ((<|>))
import Data.Text (Text)

-- ============================================================================
-- Shared expression parser setup
-- ============================================================================

-- A simple arithmetic expression parser used by most tests.
-- Operators (highest precedence first):
--   prefix ~        (negation)   -- we use '~' instead of '-' to avoid
--                                   conflict with Haskell's "--" comment syntax
--   postfix ++      (increment by 1)
--   right-assoc ^   (exponentiation)
--   left-assoc * /  (multiplication, integer division)
--   left-assoc + -  (addition, subtraction)
--   non-assoc ==    (equality: returns 1 if equal, 0 otherwise)

opTable :: [[Operator Int]]
opTable =
  [ [ Prefix  (symbol "~" *> pure negate)
    , Postfix (string "++" *> space *> pure (+1))
    ]
  , [ InfixR  (symbol "^" *> pure (^))
    ]
  , [ InfixL  (symbol "*" *> pure (*))
    , InfixL  (symbol "/" *> pure div)
    ]
  , [ InfixL  (symbol "+" *> pure (+))
    , InfixL  (symbol "-" *> pure (-))
    ]
  , [ InfixN  (symbol "==" *> pure (\a b -> if a == b then 1 else 0))
    ]
  ]

-- term: a decimal number or a parenthesised sub-expression
term :: Parser Int
term = parens <|> decimal
  where parens = character '(' *> expr <* character ')'

expr :: Parser Int
expr = buildExprParser opTable term

-- helper: parse an expression, discard position, return (result, leftover)
run :: Text -> Either [Error] (Int, Text)
run = stripPos . parse expr

-- helper: parse an expression and require EOF (nothing left over)
runFull :: Text -> Either [Error] (Int, Text)
runFull = stripPos . parse (expr <* eof)

-- helper: did the parse fail?
isFail :: Either [Error] a -> Bool
isFail (Left _)  = True
isFail (Right _) = False

-- ============================================================================
-- Tests
-- ============================================================================

tests :: Test
tests = TestList

  -- -----------------------------------------------------------------
  -- Basic arithmetic
  -- -----------------------------------------------------------------
  [ "bare number"
      ~: run "42" ~?= Right (42, "")

  , "simple addition"
      ~: run "1 + 2" ~?= Right (3, "")

  , "simple subtraction"
      ~: run "5 - 3" ~?= Right (2, "")

  , "simple multiplication"
      ~: run "3 * 4" ~?= Right (12, "")

  , "simple division"
      ~: run "10 / 3" ~?= Right (3, "")

  -- -----------------------------------------------------------------
  -- Precedence
  -- -----------------------------------------------------------------
  , "mul binds tighter than add"
      ~: run "2 + 3 * 4" ~?= Right (14, "")   -- 2 + (3*4)

  , "mul binds tighter than sub"
      ~: run "10 - 2 * 3" ~?= Right (4, "")   -- 10 - (2*3)

  , "div binds tighter than add"
      ~: run "1 + 8 / 2" ~?= Right (5, "")    -- 1 + (8/2)

  , "exponent binds tighter than mul"
      ~: run "2 * 3 ^ 2" ~?= Right (18, "")   -- 2 * (3^2)

  , "full example: ~3 + 4 * 2 ^ 2"
      ~: run "~3 + 4 * 2 ^ 2" ~?= Right (13, "")

  , "negation then exponent: ~2 ^ 2 = (~2)^2 = 4"
      ~: run "~2 ^ 2" ~?= Right (4, "")

  -- -----------------------------------------------------------------
  -- Left associativity
  -- -----------------------------------------------------------------
  , "left-assoc add: 1 + 2 + 3 = (1+2)+3 = 6"
      ~: run "1 + 2 + 3" ~?= Right (6, "")

  , "left-assoc sub: 10 - 3 - 2 = (10-3)-2 = 5"
      ~: run "10 - 3 - 2" ~?= Right (5, "")

  , "left-assoc mul: 2 * 3 * 4 = (2*3)*4 = 24"
      ~: run "2 * 3 * 4" ~?= Right (24, "")

  , "left-assoc long chain: 1 + 2 + 3 + 4 + 5 = 15"
      ~: run "1 + 2 + 3 + 4 + 5" ~?= Right (15, "")

  , "left-assoc sub proves grouping: 100 - 50 - 30 - 10 = 10"
      ~: run "100 - 50 - 30 - 10" ~?= Right (10, "")

  , "left-assoc div: 100 / 10 / 5 = (100/10)/5 = 2"
      ~: run "100 / 10 / 5" ~?= Right (2, "")

  , "mixed left-assoc at same level: 10 + 3 - 2 + 1 = 12"
      ~: run "10 + 3 - 2 + 1" ~?= Right (12, "")

  , "mixed mul/div at same level: 12 * 3 / 6 * 2 = 12"
      ~: run "12 * 3 / 6 * 2" ~?= Right (12, "")

  -- -----------------------------------------------------------------
  -- Right associativity
  -- -----------------------------------------------------------------
  , "right-assoc exponent: 2 ^ 3 ^ 2 = 2^(3^2) = 2^9 = 512"
      ~: run "2 ^ 3 ^ 2" ~?= Right (512, "")

  , "right-assoc triple: 2 ^ 2 ^ 2 ^ 2 = 2^(2^(2^2)) = 2^(2^4) = 2^16 = 65536"
      ~: run "2 ^ 2 ^ 2 ^ 2" ~?= Right (65536, "")

  , "right vs left matters: 2 ^ 3 ^ 2 = 512 (not 64)"
      ~: run "2 ^ 3 ^ 2" ~?= Right (512, "")

  -- -----------------------------------------------------------------
  -- Non-associativity (==)
  -- -----------------------------------------------------------------
  , "non-assoc == true"
      ~: run "3 == 3" ~?= Right (1, "")

  , "non-assoc == false"
      ~: run "3 == 4" ~?= Right (0, "")

  , "non-assoc == with expressions: (1+2) == 3"
      ~: run "(1 + 2) == 3" ~?= Right (1, "")

  , "non-assoc == binds looser than add: 1 + 2 == 3 => 1"
      ~: run "1 + 2 == 3" ~?= Right (1, "")

  , "non-assoc == false with exprs: 2 * 3 == 1 + 4 => 6 == 5 => 0"
      ~: run "2 * 3 == 1 + 4" ~?= Right (0, "")

  , "non-assoc == true with exprs: 2 * 3 == 1 + 5 => 6 == 6 => 1"
      ~: run "2 * 3 == 1 + 5" ~?= Right (1, "")

  , "non-assoc == chained leaves unconsumed (not a full parse)"
      ~: case runFull "1 == 2 == 3" of
           Left _ -> return ()
           Right _ -> assertFailure "chained == should fail with eof"

  -- -----------------------------------------------------------------
  -- Prefix (negation with ~)
  -- -----------------------------------------------------------------
  , "prefix negate"
      ~: run "~5" ~?= Right (-5, "")

  , "double prefix negate: ~~5 = 5"
      ~: run "~~5" ~?= Right (5, "")

  , "triple prefix negate: ~~~5 = -5"
      ~: run "~~~5" ~?= Right (-5, "")

  , "prefix with addition: ~1 + 2 = 1"
      ~: run "~1 + 2" ~?= Right (1, "")

  , "prefix binds tighter than mul: ~2 * 3 = (~2)*3 = -6"
      ~: run "~2 * 3" ~?= Right (-6, "")

  , "prefix binds tighter than exponent: ~2 ^ 2 = (~2)^2 = 4"
      ~: run "~2 ^ 2" ~?= Right (4, "")

  , "prefix binds tighter than add: ~1 + ~2 = -1 + -2 = -3"
      ~: run "~1 + ~2" ~?= Right (-3, "")

  -- -----------------------------------------------------------------
  -- Postfix (++)
  -- -----------------------------------------------------------------
  , "postfix increment: 5++ = 6"
      ~: run "5++" ~?= Right (6, "")

  , "double postfix: 5++++ = 7"
      ~: run "5++++" ~?= Right (7, "")

  , "postfix then add: 5++ + 1 = 7"
      ~: run "5++ + 1" ~?= Right (7, "")

  , "postfix with mul: 5++ * 2 = 12"
      ~: run "5++ * 2" ~?= Right (12, "")

  -- -----------------------------------------------------------------
  -- Prefix + Postfix interaction
  -- -----------------------------------------------------------------
  , "prefix then postfix: ~5++ = post(pre(5)) = (+1)(negate(5)) = -4"
      ~: run "~5++" ~?= Right (-4, "")

  , "double prefix + postfix: ~~5++ = post(pre(pre(5))) = (+1)(5) = 6"
      ~: run "~~5++" ~?= Right (6, "")

  -- -----------------------------------------------------------------
  -- Parentheses
  -- -----------------------------------------------------------------
  , "parens override precedence: (1 + 2) * 3 = 9"
      ~: run "(1 + 2) * 3" ~?= Right (9, "")

  , "nested parens: ((1 + 2)) = 3"
      ~: run "((1 + 2))" ~?= Right (3, "")

  , "deeply nested: (((((42))))) = 42"
      ~: run "(((((42)))))" ~?= Right (42, "")

  , "parens in right operand: 2 * (3 + 4) = 14"
      ~: run "2 * (3 + 4)" ~?= Right (14, "")

  , "parens both sides: (1 + 2) * (3 + 4) = 21"
      ~: run "(1 + 2) * (3 + 4)" ~?= Right (21, "")

  , "parens override right-assoc: (2 ^ 3) ^ 2 = 8^2 = 64 (not 512)"
      ~: run "(2 ^ 3) ^ 2" ~?= Right (64, "")

  , "negation of paren: ~(1 + 2) = -3"
      ~: run "~(1 + 2)" ~?= Right (-3, "")

  , "paren with postfix inside: (5++) * 2 = 12"
      ~: run "(5++) * 2" ~?= Right (12, "")

  , "parens with == inside: (1 == 1) + (2 == 3) = 1 + 0 = 1"
      ~: run "(1 == 1) + (2 == 3)" ~?= Right (1, "")

  , "complex nesting: ((2 + 3) * (4 - 1)) ^ 2 = (5*3)^2 = 15^2 = 225"
      ~: run "((2 + 3) * (4 - 1)) ^ 2" ~?= Right (225, "")

  -- -----------------------------------------------------------------
  -- Complex / tricky expressions
  -- -----------------------------------------------------------------
  , "mixed ops: 1 + 2 * 3 - 4 / 2 = 1 + 6 - 2 = 5"
      ~: run "1 + 2 * 3 - 4 / 2" ~?= Right (5, "")

  , "all ops: ~1 + 2 * 3 ^ 2 - 4 = -1 + 18 - 4 = 13"
      ~: run "~1 + 2 * 3 ^ 2 - 4" ~?= Right (13, "")

  , "exponent and mul: 2 ^ 2 * 2 ^ 2 = 4*4 = 16"
      ~: run "2 ^ 2 * 2 ^ 2" ~?= Right (16, "")

  , "subtraction vs negation: 5 - ~3 = 5 - (-3) = 8"
      ~: run "5 - ~3" ~?= Right (8, "")

  , "nested negation in subtraction: 5 - ~~3 = 5 - 3 = 2"
      ~: run "5 - ~~3" ~?= Right (2, "")

  , "complex paren nesting: (1 + (2 * (3 + 4))) = 1 + 14 = 15"
      ~: run "(1 + (2 * (3 + 4)))" ~?= Right (15, "")

  , "parens with exponent: (1 + 1) ^ (1 + 2) = 2^3 = 8"
      ~: run "(1 + 1) ^ (1 + 2)" ~?= Right (8, "")

  , "long expression: 1 + 2 * 3 + 4 * 5 + 6 = 1+6+20+6 = 33"
      ~: run "1 + 2 * 3 + 4 * 5 + 6" ~?= Right (33, "")

  , "zero: 0 + 0 = 0"
      ~: run "0 + 0" ~?= Right (0, "")

  , "negate zero: ~0 = 0"
      ~: run "~0" ~?= Right (0, "")

  , "exponent of 0: 5 ^ 0 = 1"
      ~: run "5 ^ 0" ~?= Right (1, "")

  , "exponent of 1: 5 ^ 1 = 5"
      ~: run "5 ^ 1" ~?= Right (5, "")

  , "multiply by 0: 999 * 0 = 0"
      ~: run "999 * 0" ~?= Right (0, "")

  , "precedence stress: 2 + 3 * 4 ^ 2 - 1 = 2 + 3*16 - 1 = 49"
      ~: run "2 + 3 * 4 ^ 2 - 1" ~?= Right (49, "")

  , "right-assoc exponent with add: 1 + 2 ^ 1 ^ 3 = 1 + 2^1 = 3"
      ~: run "1 + 2 ^ 1 ^ 3" ~?= Right (3, "")

  , "div and sub chain: 100 / 10 / 2 - 1 = 5-1 = 4"
      ~: run "100 / 10 / 2 - 1" ~?= Right (4, "")

  , "parens defeat right-assoc: (2 ^ 2) ^ 3 = 4^3 = 64"
      ~: run "(2 ^ 2) ^ 3" ~?= Right (64, "")

  , "no parens right-assoc: 2 ^ 2 ^ 3 = 2^8 = 256"
      ~: run "2 ^ 2 ^ 3" ~?= Right (256, "")

  , "negate in parens vs outside: ~(3) + (~3) = -6"
      ~: run "~(3) + (~3)" ~?= Right (-6, "")

  , "postfix inside exponent base: 2++ ^ 2 = 3^2 = 9"
      ~: run "2++ ^ 2" ~?= Right (9, "")

  , "everything: ~(2++ * (3 + 1)) ^ 2 == 144 => ~(3*4)^2 == 144 => ~12^2 == 144 => 144 == 144 => 1"
      ~: run "~(2++ * (3 + 1)) ^ 2 == 144" ~?= Right (1, "")

  , "subtraction chain then mul: (10 - 3 - 2) * 5 = 5*5 = 25"
      ~: run "(10 - 3 - 2) * 5" ~?= Right (25, "")

  , "exponent tower with mul: 2 * 2 ^ 3 ^ 1 + 1 = 2*8 + 1 = 17"
      ~: run "2 * 2 ^ 3 ^ 1 + 1" ~?= Right (17, "")

  -- -----------------------------------------------------------------
  -- Whitespace handling
  -- -----------------------------------------------------------------
  , "no spaces: 1+2*3"
      ~: run "1+2*3" ~?= Right (7, "")

  , "extra spaces (partial parse, trailing space is leftover)"
      ~: case run "  1  +  2  *  3  " of
           Right (7, _) -> return ()
           other -> assertFailure $ "Expected Right (7, _), got: " ++ show other

  , "leading whitespace"
      ~: run "  42" ~?= Right (42, "")

  -- -----------------------------------------------------------------
  -- Single-element operator table (minimal config)
  -- -----------------------------------------------------------------
  , "empty operator table = just term" ~:
    let e = buildExprParser ([] :: [[Operator Int]]) decimal
    in stripPos (parse e "42") ~?= Right (42, "")

  , "single prefix row only" ~:
    let e = buildExprParser [[Prefix (symbol "~" *> pure negate)]] decimal
    in stripPos (parse e "~5") ~?= Right (-5, "")

  , "single infix row only" ~:
    let e = buildExprParser [[InfixL (symbol "+" *> pure (+))]] decimal
    in stripPos (parse e "1 + 2 + 3") ~?= Right (6, "")

  , "single right-assoc row only: 1:(2:3) = 1:(23) = 33" ~:
    let e = buildExprParser [[InfixR (symbol ":" *> pure (\a b -> a * 10 + b))]] decimal
    in stripPos (parse e "1 : 2 : 3") ~?= Right (33, "")
    -- f = \a b -> a*10+b.  Right-assoc: 1:(2:3) = 1:(2*10+3) = 1:23 = 1*10+23 = 33
    -- Left-assoc would give: (1:2):3 = 12:3 = 12*10+3 = 123

  , "single non-assoc row only" ~:
    let e = buildExprParser [[InfixN (symbol "?" *> pure (\a b -> a + b))]] decimal
    in stripPos (parse e "1 ? 2") ~?= Right (3, "")

  -- -----------------------------------------------------------------
  -- INTENTIONAL FAILURES -- things that should NOT parse
  -- -----------------------------------------------------------------
  , "FAIL: empty input"
      ~: assertBool "empty input should fail" (isFail (run ""))

  , "FAIL: just an infix operator (+)"
      ~: assertBool "bare '+' should fail" (isFail (run "+"))

  , "FAIL: just an infix operator (*)"
      ~: assertBool "bare '*' should fail" (isFail (run "*"))

  , "FAIL: infix operator before term: + 1"
      ~: assertBool "'+ 1' should fail" (isFail (run "+ 1"))

  , "FAIL: infix operator before term: * 3"
      ~: assertBool "'* 3' should fail" (isFail (run "* 3"))

  , "FAIL: trailing add (full parse): 1 +"
      ~: assertBool "'1 +' should fail full parse"
         (isFail (runFull "1 +"))

  , "FAIL: trailing mul (full parse): 5 *"
      ~: assertBool "'5 *' should fail full parse"
         (isFail (runFull "5 *"))

  , "FAIL: trailing exponent: 2 ^"
      ~: assertBool "'2 ^' should fail full parse"
         (isFail (runFull "2 ^"))

  , "FAIL: double infix add (full parse): 1 + + 2"
      ~: assertBool "'1 + + 2' should fail full parse"
         (isFail (runFull "1 + + 2"))

  , "FAIL: double infix mul (full parse): 1 * * 2"
      ~: assertBool "'1 * * 2' should fail full parse"
         (isFail (runFull "1 * * 2"))

  , "FAIL: postfix in prefix position: ++5"
      ~: assertBool "'++5' should fail"
         (isFail (run "++5"))

  , "FAIL: unmatched open paren (full parse)"
      ~: assertBool "'(1 + 2' should fail full parse"
         (isFail (runFull "(1 + 2"))

  , "FAIL: unmatched close paren"
      ~: assertBool "')' should fail"
         (isFail (run ")"))

  , "FAIL: empty parens"
      ~: assertBool "'()' should fail"
         (isFail (run "()"))

  , "FAIL: mismatched parens (full parse)"
      ~: assertBool "'(1 + 2))' should fail full parse"
         (isFail (runFull "(1 + 2))"))

  , "FAIL: operator only in parens"
      ~: assertBool "'(+)' should fail"
         (isFail (run "(+)"))

  , "FAIL: consecutive numbers (full parse)"
      ~: assertBool "'1 2' should fail full parse"
         (isFail (runFull "1 2"))

  , "FAIL: chained == (full parse)"
      ~: assertBool "'1 == 2 == 3' should fail full parse"
         (isFail (runFull "1 == 2 == 3"))

  , "FAIL: letters not valid"
      ~: assertBool "'abc' should fail"
         (isFail (run "abc"))

  , "FAIL: operator in parens: (1 +)"
      ~: assertBool "'(1 +)' should fail"
         (isFail (run "(1 +)"))

  , "FAIL: nested fail: (1 + (2 *))"
      ~: assertBool "'(1 + (2 *))' should fail"
         (isFail (run "(1 + (2 *))"))

  , "FAIL: exponent with no right operand in parens: (2 ^)"
      ~: assertBool "'(2 ^)' should fail"
         (isFail (run "(2 ^)"))

  , "FAIL: only prefix op, no operand (full parse): ~"
      ~: assertBool "'~' should fail full parse"
         (isFail (runFull "~"))

  , "FAIL: == at start: == 5"
      ~: assertBool "'== 5' should fail"
         (isFail (run "== 5"))

  , "FAIL: ^ at start: ^ 5"
      ~: assertBool "'^ 5' should fail"
         (isFail (run "^ 5"))

  -- -----------------------------------------------------------------
  -- Edge: partial parse (not full parse) succeeds with leftover
  -- -----------------------------------------------------------------
  , "partial parse: '1 + 2 foo' succeeds with leftover"
      ~: case run "1 + 2 foo" of
           Right (3, _) -> return ()
           other -> assertFailure $ "Expected Right (3, _), got: " ++ show other

  , "partial parse: trailing '+' without eof succeeds, returns just 1"
      ~: case run "1 +" of
           Right (1, _) -> return ()
           other -> assertFailure $ "Expected Right (1, _), got: " ++ show other

  -- -----------------------------------------------------------------
  -- Multi-char operator edge cases
  -- -----------------------------------------------------------------
  , "multi-char op: == does not eat single =" ~:
    let tbl = [[ InfixN (symbol "==" *> pure (\a b -> if a == b then 1 else 0)) ]]
        e = buildExprParser tbl decimal
    in case stripPos (parse e "3 = 4") of
         Right (3, _) -> return ()
         other -> assertFailure $ "Expected Right (3, _), got: " ++ show other
  ]

main :: IO ()
main = do
  putStrLn "Running ExprParser tests..."
  cnts <- runTestTT tests
  putStrLn $ "\nExprParser Results: " ++ show cnts
  if errors cnts + failures cnts > 0
    then fail "Some ExprParser tests failed!"
    else return ()
