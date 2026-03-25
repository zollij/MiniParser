{-# LANGUAGE OverloadedStrings #-}

module MiniParser.Comments.Haskell where

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
