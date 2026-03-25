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
comments = pDiscard [void pELComment, void pBlockComment]

-- End-of-Line comment: "-- ..." until EOL
pELComment :: Parser Text
pELComment = do
  _ <- string "--"
  takeUntilEOL'

-- Block comment: "{- ... -}" (supports nesting)
pBlockComment :: Parser Text
pBlockComment = nestedBlockComment "{-" "-}"
