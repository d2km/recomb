module Atto2 (replaceAll) where

import Prelude hiding (takeWhile)
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.ByteString.Char8

chars = inClass "/:\\. (),'\""
good = takeTill chars
bad = takeWhile1 chars

replaceAll :: S.ByteString -> S.ByteString
replaceAll = (S.intercalate (S.pack "_")) . replace
  where
    replace s =
      case parseOnly p s of
        Right x -> x
    p = (good `sepBy1` bad) <* endOfInput
