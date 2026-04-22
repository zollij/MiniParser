-- | Expression parser for MiniParser, inspired by Parsec's buildExpressionParser.
--
-- This module lets you build a full expression parser from a simple "term"
-- parser and an operator table that specifies fixity (prefix, infix, postfix),
-- associativity (left, right, none) and operator precedence.
--
-- ============================================================================
-- RUNNING EXAMPLE (used throughout this file)
-- ============================================================================
--
-- Consider the following example expression:
-- "-3 + 4 * 2 ^ 2"
--
-- And this is our example operator table, which must have the highest precedent
-- list of operators first.
--
--   opTable :: [[Operator Int]]
--   opTable =
--     [ [ Prefix  (symbol "-" *> pure negate) ]  -- row 0: negation <- highest precedent
--     , [ InfixR  (symbol "^" *> pure (^))    ]  -- row 1: exponent
--     , [ InfixL  (symbol "*" *> pure (*))
--       , InfixL  (symbol "/" *> pure div)    ]  -- row 2: mul/div
--     , [ InfixL  (symbol "+" *> pure (+))
--       , InfixL  (symbol "-" *> pure (-))    ]  -- row 3: add/sub
--       ]
--
-- The term parser handles atomic values and parenthesised sub-expressions.
-- Parentheses are not an operator; they are part of the term parser.
-- When the term parser sees '(', it calls 'expr' recursively, which
-- re-enters the parse at the beginning precedence level.  In the case of
-- "(1 + 2) * 3", the term parser, along with the expression parser will
-- parse out "1 + 2" as a subexpression before returning it as a single node
-- to the "*" level.
--
--   term :: Parser Int
--   term = parens <|> decimal
--     where parens = character '(' *> expr <* character ')'
--
--   expr :: Parser Int
--   expr = buildExprParser opTable term
--
-- Parsing "-3 + 4 * 2 ^ 2" with 'expr' yields 13, because the operator
-- table causes it to be read as:
--
--     ((-3) + (4 * (2 ^ 2)))
--     = (-3) + (4 * 4)
--     = (-3) + 16
--     = 13
--
-- The comments in this file gives an explanation of how this happens.

module MiniParser.ExprParser (
  Operator(..),
  buildExprParser
) where

import MiniParser.Base
import Control.Applicative

-- ============================================================================
-- Operator type
-- ============================================================================

-- | The definition of an Operator, which must be one of:
--     InfixL (infix, left associative)
--     InfixR (infix, right associative)
--     InfixN (infix, no associativity)
--     Prefix
--     Postfix
-- Each of the above operators is a parser that matches on the operator
-- symbol and returns the associated function, e.g. a parse of "+" will
-- return the InfixL Parser (a -> a -> a) which does addition:
--   InfixL (symbol "+" *> pure (\x y -> x + y))
--
-- Further example Operators from our running example expression:
--
-- Prefix  (symbol "-" *> pure negate)
--   The parser (symbol "-") matches the "-" token; on success it returns
--   the 'negate' function, which will be applied to the operand.
--
-- InfixL  (symbol "*" *> pure (*))
--   The parser (symbol "*") matches "*"; on success it returns the (*)
--   function, which will be applied to left and right operands, and
--   InfixL says the operator associates to the left:
--     1 * 2 * 3  ==>  (1 * 2) * 3
--
-- InfixR  (symbol "^" *> pure (^))
--   Same idea, but InfixR means right association:
--     2 ^ 3 ^ 2  ==>  2 ^ (3 ^ 2) = 2 ^ 9 = 512
--
-- InfixN  (symbol "==" *> pure (\a b -> if a == b then 1 else 0))
--   InfixN means non-associative: "1 == 2 == 3" is a parse error.
--
-- Postfix (symbol "!" *> pure factorial)
--   The parser matches "!" after an operand; on success it returns a
--   function to apply, e.g.  5!  ==>  factorial 5  ==>  120

data Operator a
  = InfixL  (Parser (a -> a -> a))   -- left-associative binary operator
  | InfixR  (Parser (a -> a -> a))   -- right-associative binary operator
  | InfixN  (Parser (a -> a -> a))   -- non-associative binary operator
  | Prefix  (Parser (a -> a))        -- prefix unary operator
  | Postfix (Parser (a -> a))        -- postfix unary operator

-- ============================================================================
-- buildExprParser  --  the main ExprParser API
-- ============================================================================

-- | Build an expression parser from an operator table and a term parser.
--
-- The operator table is a list of rows. Each row is a list of operators
-- that share the same precedence. Rows are ordered from highest precedence
-- (tightest binding) to lowest precedence (weakest binding).
--
-- Continuing the running example:
--
-- Given:
--   opTable = [ [Prefix negate], [InfixR (^)], [InfixL (*), InfixL (/)], [InfixL (+), InfixL (-)] ]
--   term    = decimal
--
-- buildExprParser folds the table from lowest precedence to highest precedence,
-- wrapping each row around the previously generated parser. Conceptually:
--
-- Step 0:  start with 'term'                     -- parses bare numbers
-- Step 1:  wrap with row 3 [InfixL +, InfixL -]  -- can now parse "a + b"
-- Step 2:  wrap with row 2 [InfixL *, InfixL /]  -- can now parse "a * b + c"
-- Step 3:  wrap with row 1 [InfixR ^]            -- can now parse "a ^ b * c + d"
-- Step 4:  wrap with row 0 [Prefix negate]       -- can now parse "-a ^ b * c + d"
--
-- Because each step makes the previous parser its inner parser, higher-precedence
-- operators bind more tightly.
--
-- For the input "-3 + 4 * 2 ^ 2", the final parser works like this:
--
--   The outermost parser (row 0, Prefix) sees "-", applies negate,
--   and calls down into the next level for its operand.
--
--   That next level (row 1, InfixR ^) tries to parse an exponentiation.
--   Its sub-expressions are parsed by the level below it (row 2, InfixL */).
--
--   And so on -- each level only "sees" operators at its own precedence
--   or higher, which is exactly how precedence works.

buildExprParser :: [[Operator a]] -> Parser a -> Parser a
-- foldl processes the table from left (highest precedence) to right
-- (lowest precedence).  At each step, 'buildLevel row prevParser'
-- creates a new parser where 'row' operators bind more tightly than
-- anything at a lower level, because 'prevParser' (which already
-- handles higher-precedence ops) is used as the sub-expression parser.
buildExprParser table term = foldl (flip buildLevel) term table

-- ============================================================================
-- buildLevel  --  handle one precedence level
-- ============================================================================

-- | Given a row of operators (all at the same precedence) and a parser
-- for sub-expressions (which handles everything at higher precedence),
-- produce a parser for this level.
--
-- Example: Processing row 2 [InfixL (*), InfixL (/)]
--
-- Input to parse at this point: "4 * 2 ^ 2"
-- 'subExpr' already knows how to parse "2 ^ 2" (= 4) because it
-- handles rows 0 and 1.
--
-- buildLevel first separates the operators by kind (Prefix, Postfix,
-- InfixL, InfixR, InfixN), then builds a parser that:
--
-- 1. Applies any prefix operators (none in this row)
-- 2. Parses a sub-expression:  "4"  => 4
-- 3. Applies any postfix operators (none in this row)
-- 4. Looks for infix operators and additional operands:
--      sees "*", then parses sub-expression "2 ^ 2" => 4
--      result: 4 * 4 = 16

buildLevel :: [Operator a] -> Parser a -> Parser a
buildLevel ops subExpr = do
  -- Apply zero or more prefix operators, then parse a sub-expression,
  -- then apply zero or more postfix operators.  This gives us the
  -- first operand (possibly decorated with unary operators).
  x <- pTerm
  -- Now look for infix operators at this level.
  infixRest x
  where
    -- Split the operators in this row by kind.
    --
    -- Example: For row 2 = [InfixL (*), InfixL (/)]:
    --   infixLOps  = [(*), (/)]    -- parsers that match "*" or "/"
    --   infixROps  = []
    --   infixNOps  = []
    --   prefixOps  = []
    --   postfixOps = []
    (infixLOps, infixROps, infixNOps, prefixOps, postfixOps) = splitOps ops

    -- Parser for one prefix operator (tries each prefix op in this row).
    -- Returns the function to apply (e.g. 'negate').
    prefixOp  = choice prefixOps

    -- Parser for one postfix operator.
    postfixOp = choice postfixOps

    -- Parser for a "term at this level":
    --   1. Collect prefix operators (zero or more)
    --   2. Parse a sub-expression (which handles higher-precedence ops)
    --   3. Collect postfix operators (zero or more)
    --
    -- EXAMPLE: For row 0 [Prefix negate], parsing "-3":
    --   prefixOp matches "-", returns 'negate'
    --   subExpr parses "3", returns 3
    --   negate applied to 3 gives (-3)
    --
    -- Multiple prefixes chain: "--3" => negate (negate 3) = 3
    -- Multiple postfixes chain: "3!!" => factorial (factorial 3) = 720
    pTerm = do
      pre  <- composeMany prefixOp   -- e.g. negate . negate . id
      x    <- subExpr                -- e.g. 3
      post <- composeMany postfixOp  -- e.g. factorial . id
      return (post (pre x))          -- apply both: post(pre(x))

    -- After parsing the first operand, look for an infix operator.
    -- Try left-associative, right-associative, and non-associative
    -- in turn. If none match, just return the operand as-is.
    --
    -- Each of infixLFirst, infixRRest, infixNRest must only succeed
    -- when they actually match an operator. The "no operator
    -- found" fallback (pure x) lives here in infixRest, not inside
    -- the individual helpers. If infixLFirst had its own 'pure x'
    -- fallback, it would always succeed, and infixRRest / infixNRest
    -- would never be reached (because <|> short-circuits on success).
    --
    -- Example: For row 3 [InfixL (+), InfixL (-)], after parsing
    -- the left operand (which came out as 16 from the higher levels):
    --
    -- Input remaining: "+ 4 * 2 ^ 2"
    -- infixLFirst tries InfixL parsers, finds "+", then infixLCont
    -- recurses to handle "4 * 2 ^ 2" as the right operand.
    infixRest x = infixLFirst x
              <|> infixRRest x
              <|> infixNRest x
              <|> pure x        -- no infix operator found; return x as-is

    -- -----------------------------------------------------------------------
    -- Left-associative infix:  a + b + c  ==>  (a + b) + c
    -- -----------------------------------------------------------------------
    -- infixLFirst: match the FIRST left-assoc operator.  This must fail
    -- (not fall back to pure x) when no operator is found, so that
    -- infixRest can try right-assoc and non-assoc alternatives.
    --
    -- infixLCont: after the first operator matched, LOOP looking for
    -- more operators of the same kind.  This one CAN fall back to
    -- pure x, because we've already committed to left-assoc parsing.
    --
    -- EXAMPLE: Parsing "1 + 2 + 3" at the add/sub level:
    --   infixLFirst: x = 1
    --     see "+", parse right operand => 2,  apply: (1 + 2) = 3
    --     infixLCont 3:
    --       see "+", parse right operand => 3,  apply: (3 + 3) = 6
    --       infixLCont 6: no more "+"  =>  pure 6  =>  return 6
    infixLFirst x = do
      f <- choice infixLOps  -- parse the operator, e.g. "+" => (+)
      y <- pTerm             -- parse the right operand
      infixLCont (f x y)     -- apply and continue: infixLCont (x + y)

    infixLCont x = infixLFirst x  -- try for another left-assoc op
              <|> pure x          -- no more => done

    -- -----------------------------------------------------------------------
    -- Right-associative infix:  2 ^ 3 ^ 2  ==>  2 ^ (3 ^ 2)
    -- -----------------------------------------------------------------------
    -- Parse one operator, then recursively parse everything to the right
    -- (which naturally groups to the right because the recursive call
    -- will grab more ^s before returning).
    --
    -- EXAMPLE: Parsing "2 ^ 3 ^ 2" at the exponent level:
    --   x = 2
    --   see "^", then recursively parse "3 ^ 2":
    --     x = 3
    --     see "^", then recursively parse "2":
    --       x = 2, no more "^" => return 2
    --     apply: 3 ^ 2 = 9
    --   apply: 2 ^ 9 = 512
    infixRRest x = do
      f <- choice infixROps   -- parse the operator, e.g. "^" => (^)
      y <- pTerm              -- parse the immediate next operand
      z <- infixRRest y       -- recursively grab more right-assoc ops
            <|> pure y        -- or just return y if no more
      return (f x z)          -- apply: x ^ (rest)

    -- -----------------------------------------------------------------------
    -- Non-associative infix:  1 == 2  is OK,  1 == 2 == 3  is an error
    -- -----------------------------------------------------------------------
    -- Parse exactly one operator and one right operand.  Do NOT recurse.
    --
    -- EXAMPLE: Parsing "1 == 2":
    --   x = 1
    --   see "==", parse right operand => 2
    --   apply the comparison function to (1, 2)
    --   If input continued with "== 3", the OUTER parser would fail
    --   because "==" is not a valid start for a higher-precedence term.
    infixNRest x = do
      f <- choice infixNOps   -- parse the operator
      y <- pTerm              -- parse the right operand
      return (f x y)          -- apply once; do NOT look for more

-- ============================================================================
-- Helper: compose zero or more unary operators
-- ============================================================================

-- | Parse zero or more unary operators and compose them into a single function.
-- Returns 'id' if no operators match.
--
-- EXAMPLE: parsing "--3" with Prefix negate:
--   First call:  prefixOp matches "-", returns negate
--   Second call: prefixOp matches "-", returns negate
--   Third call:  prefixOp fails (next char is '3'), returns id
--   Result: negate . negate . id  (which, applied to 3, gives 3)
--
-- EXAMPLE: parsing "3!!" with Postfix factorial:
--   First call:  postfixOp matches "!", returns factorial
--   Second call: postfixOp matches "!", returns factorial
--   Third call:  postfixOp fails (next char is space/EOF), returns id
--   Result: factorial . factorial . id  (applied to 3 => factorial(6) = 720)

composeMany :: Parser (a -> a) -> Parser (a -> a)
-- 'many p' collects [f1, f2, ...] until p fails.
-- 'foldr (.) id' composes them:  f1 . f2 . ... . id
-- If none matched, many returns [], and foldr (.) id [] = id.
composeMany p = foldr (.) id <$> many p

-- ============================================================================
-- Helper: split an operator list by kind
-- ============================================================================

-- | splitOps artitions a list of Operator values into five groups by fixity.
-- Each group contains just the parser, without the Operator wrapper.
--
-- Example: splitOps [InfixL p1, Prefix p2, InfixL p3, Postfix p4]
--   => (infixL = [p1, p3], infixR = [], infixN = [], prefix = [p2], postfix = [p4])

splitOps :: [Operator a]
         -> ( [Parser (a -> a -> a)]   -- InfixL parsers
            , [Parser (a -> a -> a)]   -- InfixR parsers
            , [Parser (a -> a -> a)]   -- InfixN parsers
            , [Parser (a -> a)]        -- Prefix parsers
            , [Parser (a -> a)]        -- Postfix parsers
            )
splitOps = foldr addOp ([], [], [], [], [])
  where
    addOp (InfixL  p) (il, ir, inn, pre, post) = (p:il, ir,   inn,  pre,   post)
    addOp (InfixR  p) (il, ir, inn, pre, post) = (il,   p:ir, inn,  pre,   post)
    addOp (InfixN  p) (il, ir, inn, pre, post) = (il,   ir,   p:inn, pre,  post)
    addOp (Prefix  p) (il, ir, inn, pre, post) = (il,   ir,   inn,  p:pre, post)
    addOp (Postfix p) (il, ir, inn, pre, post) = (il,   ir,   inn,  pre,   p:post)

-- ============================================================================
-- FULL WORKED EXAMPLE: Parsing "-3 + 4 * 2 ^ 2"
-- ============================================================================
--
-- Setup (repeated from top of file for easy reference):
--
--   opTable :: [[Operator Int]]
--   opTable =
--     [ [ Prefix  (symbol "-" *> pure negate) ]  -- row 0: negation <- highest precedent
--     , [ InfixR  (symbol "^" *> pure (^))    ]  -- row 1: exponent
--     , [ InfixL  (symbol "*" *> pure (*))
--       , InfixL  (symbol "/" *> pure div)    ]  -- row 2: mul/div
--     , [ InfixL  (symbol "+" *> pure (+))
--       , InfixL  (symbol "-" *> pure (-))    ]  -- row 3: add/sub
--       ]
--
--     term = decimal
--     expr = buildExprParser opTable term
--
-- After buildExprParser folds the table, we get a tower of parsers:
--
--     level3  = buildLevel [InfixL +, InfixL -]  level2
--     level2  = buildLevel [InfixL *, InfixL /]  level1
--     level1  = buildLevel [InfixR ^]            level0
--     level0  = buildLevel [Prefix negate]        term
--
-- Parsing starts at level3 (the outermost / lowest-precedence level).
--
-- INPUT: "-3 + 4 * 2 ^ 2"
--         ^
--
-- LEVEL 3 (InfixL +/-):
--   Calls pTerm, which has no prefix/postfix ops at this level,
--   so it calls down to level2.
--
--   LEVEL 2 (InfixL */÷):
--     Calls pTerm -> level1.
--
--     LEVEL 1 (InfixR ^):
--       Calls pTerm -> level0.
--
--       LEVEL 0 (Prefix -):
--         pTerm: prefixOp matches "-", returns negate.
--                Calls subExpr = term = decimal.
--                decimal parses "3", returns 3.
--                No postfix ops.
--                Result: negate 3 = -3
--
--       Back in LEVEL 1: x = -3
--         infixRRest: no "^" found.
--         Returns -3.
--
--     Back in LEVEL 2: x = -3
--       infixLRest: no "*" or "/" found.
--       Returns -3.
--
--   Back in LEVEL 3: x = -3
--     infixLRest: sees "+".
--       INPUT: "4 * 2 ^ 2"
--              ^
--       Parses right operand via pTerm -> level2:
--
--       LEVEL 2 (InfixL */÷):
--         Calls pTerm -> level1.
--
--         LEVEL 1 (InfixR ^):
--           Calls pTerm -> level0.
--
--           LEVEL 0 (Prefix -):
--             No "-" prefix. Calls term.
--             decimal parses "4", returns 4.
--
--           Back in LEVEL 1: x = 4
--             infixRRest: no "^". Returns 4.
--
--         Back in LEVEL 2: x = 4
--           infixLRest: sees "*".
--             INPUT: "2 ^ 2"
--                    ^
--             Parses right operand via pTerm -> level1:
--
--             LEVEL 1 (InfixR ^):
--               Calls pTerm -> level0 -> term.
--               decimal parses "2", returns 2.
--               infixRRest: sees "^".
--                 INPUT: "2"
--                        ^
--                 Parses right operand: decimal parses "2", returns 2.
--                 infixRRest: no more "^". Returns 2.
--                 Result: 2 ^ 2 = 4
--
--             Back in LEVEL 2: right operand = 4
--               Apply: 4 * 4 = 16
--               infixLRest 16: no more "*" or "/". Returns 16.
--
--       Back in LEVEL 3: right operand = 16
--         Apply: (-3) + 16 = 13
--         infixLRest 13: no more "+" or "-". Returns 13.
--
-- RESULT: 13
--
-- ============================================================================
-- ADDITIONAL EXAMPLES
-- ============================================================================
--
-- RIGHT ASSOCIATIVITY:  "2 ^ 3 ^ 2"
--   Level 1 (InfixR ^):
--     x = 2, sees "^"
--     Parses right side: x = 3, sees "^"
--       Parses right side: x = 2, no more "^", returns 2
--       Apply: 3 ^ 2 = 9
--     Apply: 2 ^ 9 = 512
--   Result: 512  (not 8 ^ 2 = 64, which left-assoc would give)
--
-- NON-ASSOCIATIVITY:  "1 == 2" vs "1 == 2 == 3"
--   If we added a row: [ InfixN (symbol "==" *> pure compareEq) ]
--   "1 == 2" works fine: parseEq matches "==", parses 2, returns compareEq 1 2.
--   "1 == 2 == 3" fails: after parsing "1 == 2", the second "==" is not
--   consumed by infixNRest (it only takes one operator), and "==" is not
--   valid at any higher precedence level, so the parse fails.
--
-- PREFIX + POSTFIX on same level:
--   opTable = [ [Prefix negate, Postfix factorial], ... ]
--   "-5!" means factorial(negate(5)) = factorial(-5).
--   To get negate(factorial(5)), use parentheses: "-(5!)", or put them
--   on separate precedence rows with postfix higher than prefix.
--
-- MULTIPLE PREFIX:  "--3"
--   composeMany collects [negate, negate], composes to negate . negate.
--   Applied to 3: negate(negate(3)) = 3.
--
-- PARENTHESES:  "(1 + 2) * 3"
--   Parentheses are not part of the operator table.  They live in the
--   term parser:
--     term = parens <|> decimal
--       where parens = character '(' *> expr <* character ')'
--
--   Parsing "(1 + 2) * 3" at the mul/div level:
--     pTerm calls subExpr -> ... -> eventually reaches 'term'.
--     term tries 'parens' first: sees '(', then calls 'expr' recursively.
--       expr re-enters the full precedence "tower" from the top (level 3).
--       Inside the parens, level 3 parses "1 + 2":
--         x = 1, sees "+", right operand = 2, apply: 1 + 2 = 3
--       character ')' consumes the closing paren.
--       Returns 3 back to the outer term.
--     Back in the mul/div level: x = 3
--       infixLRest: sees "*", right operand = 3 (via decimal).
--       Apply: 3 * 3 = 9
--   Result: 9
--
--   This works because 'expr' and 'term' are mutually recursive:
--   expr uses term (via buildExprParser), and term uses expr (via parens).
--   The parentheses effectively "reset" precedence; anything inside
--   them is parsed as a fresh top-level expression.
