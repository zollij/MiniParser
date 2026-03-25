{-# LANGUAGE OverloadedStrings #-}

module MiniParser.Comments.C where

import MiniParser.Base
import Data.Text (Text)
import Control.Monad (void)

comments :: Parser ()
comments = pDiscard [void pELComment, void pInlineComment]

-- Deal with inline comments that look like /* ... */
pInlineComment :: Parser Text
pInlineComment = do
  _ <- string "/*"
  takeUntilStr' "*/"

-- End-of-Line comment (ELComment) ::= "//" ++ any text ++ EOL
-- ('\n', '\r', or some combination)
pELComment :: Parser Text
pELComment = do
  _ <- string "//"
  takeUntilEOL'
