{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Pcre
import qualified Light
import qualified Tdfa
import qualified Utils
import qualified Atto
import qualified Parsec

-- src/Gen.hs defines Arbitrary instance for ByteString
import Gen

-- ensure that naive and pre-compiled regex versions of Pcre module
-- produce the same result
prop_PcreNaive_eq_PcreCompiled s = monadicIO $ do
  re <- run Pcre.compiledRe
  s' <- run (Pcre.replaceAll' re s)
  let s'' = Pcre.replaceAll s in
    assert $ s' == s''

-- ensure that results produced by the naive version of Pcre module match
-- with those obtained by using module Light (package pcre-light)
prop_PcreNaive_eq_Light  s = s' == s''
  where s' = Pcre.replaceAll s
        s'' = Light.replaceAll s

-- ... module Tdfa (package regex-tdfa)
prop_PcreNaive_eq_Tdfa s = s' == s''
  where s' = Pcre.replaceAll s
        s'' = Tdfa.replaceAll s

-- ... module Utils (package pcre-utils)
prop_PcreNaive_eq_Utils s = s' == s''
  where s' = Pcre.replaceAll s
        s'' = Utils.replaceAll s

-- ... module Atto (package attoparsec)
prop_PcreNaive_eq_Atto s = s' == s''
  where s' = Pcre.replaceAll s
        s'' = Atto.replaceAll s

-- ... module Parsec (package parsec)
prop_PcreNaive_eq_Parsec s = s' == s''
  where s' = Pcre.replaceAll s
        s'' = Parsec.replaceAll s

return []
runTests = $quickCheckAll

main = runTests
