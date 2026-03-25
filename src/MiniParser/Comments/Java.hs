{-# LANGUAGE OverloadedStrings #-}

-- Java comment parser for MiniParser.
--
-- Java uses the same comment syntax as Jack/C with all three styles:
--   //          End-of-line comment
--   /* ... */   Inline (block) comment
--   /** ... */  Javadoc API comment
--
-- To use this as your project's comment parser, copy this file to:
--   src/MiniParser/Comments.hs
-- and change the module declaration to:
--   module MiniParser.Comments (comments) where
--
-- Or import it directly in your own parsers:
--   import qualified MiniParser.Comments.Java as JV
--   myToken p = do { JV.comments; p }

module MiniParser.Comments.Java where

import MiniParser.Base
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)

-- | Strip all whitespace and Java comments.
-- Parsers are tried in order: Javadoc ("/**") must come before
-- inline ("/*") to avoid a false match on the leading "/*".
comments :: Parser ()
comments = pDiscard [void pELComment, void pJavadocComment, void pInlineComment]

-- | End-of-line comment: "//" followed by text until EOL.
-- Example: // this is a comment
pELComment :: Parser Text
pELComment = do
  _ <- string "//"
  takeUntilEOL'

-- | Javadoc comment: "/** ... */".
-- Returns the cleaned, non-empty lines between the delimiters.
-- Leading/trailing whitespace and blank lines are stripped from each line.
pJavadocComment :: Parser [Text]
pJavadocComment = do
  _ <- string "/**"
  content <- takeUntilStr' "*/"
  let contentLines = T.lines content
  let cleanedLines = map (T.dropWhile isSpace . T.dropWhileEnd isSpace) contentLines
  let nonEmptyLines = filter (not . T.null) cleanedLines
  return nonEmptyLines

-- | Inline (block) comment: "/* ... */".
-- Rejects "/**" so that Javadoc comments are not consumed as inline comments.
-- Must be listed after pJavadocComment in the pDiscard list.
pInlineComment :: Parser Text
pInlineComment = do
  _ <- string "/*"
  c <- lookAhead
  case c of
    '*' -> pFail [Unexpected "/*" "/**"]  -- reject Javadoc; let pJavadocComment handle it
    _   -> takeUntilStr' "*/"
