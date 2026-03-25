{-# LANGUAGE OverloadedStrings #-}

module MiniParser.Comments.Jack where

import MiniParser.Base
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)

comments :: Parser ()
comments = pDiscard [void eolComment, void apiComment, void inlineComment]

-- End-of-Line comment: "// ..." until EOL
eolComment :: Parser Text
eolComment = do
  _ <- string "//"
  takeUntilEOL'

-- API comment: "/** ... */"
apiComment :: Parser [Text]
apiComment = do
  _ <- string "/**"
  content <- takeUntilStr' "*/"
  let contentLines = T.lines content
  let cleanedLines = map (T.dropWhile isSpace . T.dropWhileEnd isSpace) contentLines
  let nonEmptyLines = filter (not . T.null) cleanedLines
  return nonEmptyLines

-- Inline comment: "/* ... */"
-- Must be tried after apiComment since "/**" starts with "/*"
inlineComment :: Parser Text
inlineComment = do
  _ <- string "/*"
  c <- lookAhead
  case c of
    '*' -> pFail [Unexpected "/*" "/**"]
    _   -> takeUntilStr' "*/"
