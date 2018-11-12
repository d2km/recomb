{-# LANGUAGE OverloadedStrings #-}
module Utils (replaceAll) where

import qualified Data.ByteString.Char8 as S
import Text.Regex.PCRE.ByteString.Utils

Right re = compile' compBlank execBlank "[/:\\\\. (),'\"]+"

replaceAll :: S.ByteString -> S.ByteString
replaceAll s = case substitute' re s "_" of
                 Right result -> result
