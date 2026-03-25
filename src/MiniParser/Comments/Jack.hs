{-# LANGUAGE OverloadedStrings #-}

module MiniParser.Comments.Jack where

import MiniParser.Base
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)

comments :: Parser ()
comments = pDiscard [void pELComment, void pAPIComment, void pInlineComment]

-- End-of-Line comment: "// ..." until EOL
pELComment :: Parser Text
pELComment = do
  _ <- string "//"
  takeUntilEOL'

-- API comment: "/** ... */"
pAPIComment :: Parser [Text]
pAPIComment = do
  _ <- string "/**"
  content <- takeUntilStr' "*/"
  let contentLines = T.lines content
  let cleanedLines = map (T.dropWhile isSpace . T.dropWhileEnd isSpace) contentLines
  let nonEmptyLines = filter (not . T.null) cleanedLines
  return nonEmptyLines

-- Inline comment: "/* ... */"
-- Must be tried after pAPIComment since "/**" starts with "/*"
pInlineComment :: Parser Text
pInlineComment = do
  _ <- string "/*"
  c <- lookAhead
  case c of
    '*' -> pFail [Unexpected "/*" "/**"]
    _   -> takeUntilStr' "*/"
