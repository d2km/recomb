module Atto (replaceAll) where

import Prelude hiding (takeWhile)
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.ByteString.Char8

chars = inClass "/:\\. (),'\""
good = takeTill chars
bad = takeWhile chars

replaceAll :: S.ByteString -> S.ByteString
replaceAll = S.concat . replace
  where
    parse_good s =
      case parse good s of
        Done left x ->
          if S.null left then [x] else x : (parse_bad left)
        Partial _ ->
          [s]

    parse_bad s =
      case parse bad s of
        Done left _ ->
          if S.null left then [] else S.pack "_" : (parse_good left)
        Partial _ ->
          [S.pack "_"]

    replace = parse_good
