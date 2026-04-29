-- Floating-point parsers, polymorphic over RealFrac.
-- Kept in a separate module from MiniParser.Parser because mhs's compile
-- pass on test/Test.hs is right at its stack limit (see CHANGELOG 0.4.1.0
-- for the analogous PosTests split). Adding any top-level definition to
-- MiniParser.Parser pushes mhs over the threshold; isolating these here
-- keeps test/Test.hs's import surface unchanged.
module MiniParser.Float (fp, float, expFloat) where

import MiniParser.Base
import MiniParser.Parser (token, digits)
import Control.Applicative
import qualified Data.Text as T
import Numeric (readFloat)

-- floating point, eats comments. Default cap is 4 exponent digits, which
-- covers the entire Double range (~1e308). If you need a different cap, use
-- expFloat directly. Most users will want this parser.
float :: RealFrac f => Parser f
float = expFloat 4

-- floating point, eats comments
-- The Int argument is the maximum number of digits allowed in the exponent
-- portion of the input (e.g. expLen=4 accepts "1e9999" but rejects "1e10000").
-- The cap defends against DoS via huge intermediate Integers inside readFloat.
-- Most users won't need expFloat and will just use "float".
expFloat :: RealFrac f => Int -> Parser f
expFloat expLen = token $ fp expLen

-- raw floating point parser (doesn't eat comments).
-- The exponent length cap is enforced as a HARD failure: exceeding it rejects
-- the whole input rather than silently dropping the exponent. An absent or
-- ill-formed exponent prefix (e.g. "3e", "3eX") is still tolerated and parses
-- as the leading number with the rest left in the remainder.
fp :: RealFrac r => Int -> Parser r
fp expLen = do
  whole  <- digits
  frac   <- (T.cons '.' <$> (char '.' *> digits)) <|> pure T.empty
  -- pExp succeeds only when e/E is followed by an (optional) sign and at
  -- least one digit. We keep the "exponent is optional" backtrack but lift
  -- the cap check OUTSIDE the <|> so exceeding the cap is a hard fail.
  expoR  <- (Just <$> pExp) <|> pure Nothing
  expo   <- case expoR of
    Just (n, _)      | n > expLen ->
      pFailStr ("exponent length (" ++ show n ++ ") > " ++ show expLen)
    Just (_, lexeme) -> pure lexeme
    Nothing          -> pure T.empty
  let str = T.unpack (whole <> frac <> expo)
  case readFloat str of
    [(x, "")] -> pure x
    _fail     -> pFailStr ("invalid floating point number: " ++ str)
  where
    pExp = do
      e    <- char 'e' <|> char 'E'
      sign <- (T.singleton <$> (char '+' <|> char '-')) <|> pure T.empty
      ds   <- digits
      pure (T.length ds, T.cons e (sign <> ds))
