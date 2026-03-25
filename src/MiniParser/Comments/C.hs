{-# LANGUAGE OverloadedStrings #-}

module MiniParser.Comments.C where

import MiniParser.Base
import Data.Text (Text)
import Control.Monad (void)

comments :: Parser ()
comments = pDiscard [void eolComment, void inlineComment]

-- Deal with inline comments that look like /* ... */
inlineComment :: Parser Text
inlineComment = do
  _ <- string "/*"
  takeUntilStr' "*/"

-- End-of-Line comment (ELComment) ::= "//" ++ any text ++ EOL
-- ('\n', '\r', or some combination)
eolComment :: Parser Text
eolComment = do
  _ <- string "//"
  takeUntilEOL'
