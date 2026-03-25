-- Default comment parser: Haskell comments (-- and {- -}).
-- To handle a different language's comments, replace this file with one of:
--   MiniParser.Comments.C
--   MiniParser.Comments.Jack
--   MiniParser.Comments.Java
--   MiniParser.Comments.Kotlin
-- or write your own module that exports a 'comments :: Parser ()'.
{-# LANGUAGE OverloadedStrings #-}

module MiniParser.Comments (comments) where

import MiniParser.Base
import Data.Text (Text)
import Control.Monad (void)

comments :: Parser ()
comments = pDiscard [void eolComment, void blockComment]

-- End-of-Line comment: "-- ..." until EOL
eolComment :: Parser Text
eolComment = do
  _ <- string "--"
  takeUntilEOL'

-- Block comment: "{- ... -}" (supports nesting)
blockComment :: Parser Text
blockComment = nestedBlockComment "{-" "-}"
