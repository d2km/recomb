module Gen (arbitrarySource) where

import qualified Data.ByteString.Char8 as S
import Test.QuickCheck

-- give symbols to replace more probability to appear
replace_chars = map (\x -> (5, return x)) "/:\\. (),'\""

-- other symbols that are likely to appear in a source
letters_and_digits = map (\x -> (1, return x)) $
                     ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ ['_', '-']

instance Arbitrary S.ByteString where
  arbitrary = sized str
   where
     str n = do k <- choose (0, n)
                cs <- vectorOf k (frequency chars)
                return $ S.pack cs
     chars = replace_chars ++ letters_and_digits

-- generate a list of size n of arbitrary source strings
arbitrarySource :: Int -> IO [S.ByteString]
arbitrarySource n = generate (vectorOf n arbitrary)
