{-# LANGUAGE OverloadedStrings #-}
module Tdfa
  ( replaceAll
  ) where

import qualified Data.ByteString.Char8 as S
import Data.Array ((!))
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

execOpts =  ExecOption{captureGroups = False}
compOpts =  blankCompOpt

Right re = compile compOpts execOpts "[/:\\\\. (),'\"]+"

replaceAll :: S.ByteString -> S.ByteString
replaceAll = S.concat . replace
  where
    replace s =
      let Right result = execute re s in
        case result of
          Just a ->
            let before = S.take (fst (a!0)) s
                after = S.drop (fst (a!0) + snd (a!0)) s
            in
              before : "_" : replace after
          Nothing ->
            [s]
