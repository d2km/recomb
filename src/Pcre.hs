module Pcre
  (compiledRe
  , replaceAll
  , replaceAll'
  ) where

import qualified Data.ByteString.Char8 as S
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString
import Data.Array ((!))

import Control.DeepSeq (NFData, rnf)

-- this instance is needed for criterion/env
instance NFData Regex where
  rnf r = seq r ()

re = S.pack "[/:\\\\. (),'\"]+"
underscore = S.pack "_"

type Match = (S.ByteString, S.ByteString, S.ByteString)
type Op = S.ByteString -> Match

-- replaceAll is a naive approach that (in a straighforward way)
-- recursively applies (=~) and concatenates the resulting string
replaceAll :: S.ByteString -> S.ByteString
replaceAll = (S.intercalate underscore) . (replace ( =~ re))
  where
    replace :: Op -> S.ByteString -> [S.ByteString]
    replace op source =
      let (before, match, after) = op source
      in  if S.null match
          then [before]
          else before : (replace op after)

compiledRe :: IO Regex
compiledRe = do
  Right re <- compile compBlank execBlank re
  return re

replaceAll' :: Regex -> S.ByteString -> IO S.ByteString
replaceAll' re s = do
  pieces <- replace re s
  return $ S.concat pieces
  where
    replace re s' = do
        Right result <- execute re s'
        case result of
          Just a ->
            let before = S.take (fst (a!0)) s'
                after = S.drop (fst (a!0) + snd (a!0)) s'
            in do
              rest <- replace re after
              return $ before : underscore : rest
          Nothing ->
            return [s']
