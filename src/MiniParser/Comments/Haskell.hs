{-# LANGUAGE OverloadedStrings #-}

module MiniParser.Comments.Haskell where

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
