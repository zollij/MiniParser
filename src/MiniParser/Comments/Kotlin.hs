{-# LANGUAGE OverloadedStrings #-}

-- Kotlin comment parser for MiniParser.
--
-- Kotlin uses C-family comment syntax with three styles:
--   //          End-of-line comment
--   /* ... */   Block comment (supports nesting)
--   /** ... */  KDoc API comment
--
-- Unlike C and Java, Kotlin block comments nest:
--   /* outer /* inner */ still comment */ <-- valid Kotlin
--
-- To use this as your project's comment parser, copy this file to:
--   src/MiniParser/Comments.hs
-- and change the module declaration to:
--   module MiniParser.Comments (comments) where
--
-- Or import it directly in your own parsers:
--   import qualified MiniParser.Comments.Kotlin as KT
--   myToken p = do { KT.comments; p }

module MiniParser.Comments.Kotlin where

import MiniParser.Base
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)

-- | Strip all whitespace and Kotlin comments.
-- Parsers are tried in order: KDoc ("/**") must come before
-- block ("/*") to avoid a false match on the leading "/*".
comments :: Parser ()
comments = pDiscard [void pELComment, void pKDocComment, void pBlockComment]

-- | End-of-line comment: "//" followed by text until EOL.
pELComment :: Parser Text
pELComment = do
  _ <- string "//"
  takeUntilEOL'

-- | KDoc comment: "/** ... */".
-- Returns the cleaned, non-empty lines between the delimiters.
pKDocComment :: Parser [Text]
pKDocComment = do
  _ <- string "/**"
  content <- takeUntilStr' "*/"
  let contentLines = T.lines content
  let cleanedLines = map (T.dropWhile isSpace . T.dropWhileEnd isSpace) contentLines
  let nonEmptyLines = filter (not . T.null) cleanedLines
  return nonEmptyLines

-- | Block comment: "/* ... */" (supports nesting).
-- Rejects "/**" so that KDoc comments are not consumed as block comments.
-- Must be listed after pKDocComment in the pDiscard list.
pBlockComment :: Parser Text
pBlockComment = do
  _ <- string "/*"
  c <- lookAhead
  case c of
    '*' -> pFail [Unexpected "/*" "/**"]
    _ -> P $ \(PState pos inp) ->
      let
        go :: Int -> Int -> Pos -> Text -> Either [Error] (Text, PState)
        go cnt depth p remain
          | "*/" `T.isPrefixOf` remain =
              let
                p' = advanceText ("*/" :: Text) p
                remain' = T.drop 2 remain
              in
                if depth == 1
                then Right (T.take cnt inp, PState p' remain')
                else go (cnt + 2) (depth - 1) p' remain'
          | "/*" `T.isPrefixOf` remain =
              let
                p' = advanceText ("/*" :: Text) p
                remain' = T.drop 2 remain
              in
                go (cnt + 2) (depth + 1) p' remain'
          | otherwise =
            case T.uncons remain of
              Nothing -> Left [EndOfInput]
              Just (c', remain') ->
                go (cnt + 1) depth (advanceChar c' p) remain'
      in go 0 1 pos inp
