{-# LANGUAGE OverloadedStrings #-}
module Light
  ( replaceAll
  ) where

import qualified Data.ByteString.Char8 as S
import Text.Regex.PCRE.Light

re = compile "[/:\\\\. (),'\"]+" []


replaceAll :: S.ByteString -> S.ByteString
replaceAll = S.concat . replace
  where
    replace s =
      case match re s [] of
        Just [x] ->
          let (h, t) = S.breakSubstring x s in
            h : "_" : replace (S.drop (S.length x) t)
        Nothing ->
          [s]
