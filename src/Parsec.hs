module Parsec (replaceAll) where

import qualified Data.ByteString.Char8 as S
import Text.Parsec
import Text.Parsec.ByteString (Parser)

chars = "/:\\. (),'\""
good :: Parser [Char]
good = many1 (noneOf chars)

bad :: Parser [Char]
bad = many1 (oneOf chars)


replaceAll :: S.ByteString -> S.ByteString
replaceAll = S.concat . replace
  where
    parse_good s | S.null s = []
    parse_good s =
      case parse good "" s of
        Right xs ->
          let l = length xs in
            if S.length s == l then [s] else (S.pack xs) : (parse_bad (S.drop l s))
        Left _ ->
          parse_bad s

    parse_bad s | S.null s = []
    parse_bad s =
      case parse bad "" s of
        Right xs ->
          (S.pack "_") : (parse_good (S.drop (length xs) s))
        Left _ ->
          parse_good s

    replace = parse_good
