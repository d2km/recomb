module ParsecV2 (replaceAll) where

import qualified Data.ByteString.Char8 as S
import Text.Parsec
import Text.Parsec.ByteString (Parser)

chars = "/:\\. (),'\""

good :: Parser S.ByteString
good = many (noneOf chars) >>= (\xs -> return $ S.pack xs)

bad :: Parser ()
bad = skipMany1 (oneOf chars)

replaceAll :: S.ByteString -> S.ByteString
replaceAll = (S.intercalate (S.pack "_")) . replace
  where
    replace s = case parse p "" s of
                  Right bs -> bs
                  -- crash on `Left error` (which should never happen)
    p = (good `sepBy1` bad)
