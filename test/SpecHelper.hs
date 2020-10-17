module SpecHelper (
  module Test.Hspec,
  module Test.QuickCheck,
  genPair, genChar, genString
) where

import Test.Hspec
import Test.QuickCheck

genPair :: a -> a -> Gen (a, a)
genPair left right = pure (left, right)

genChar :: Gen Char
genChar = elements ['a'..'z']

genString :: Int -> Gen String
genString length = do
  str <- listOf genChar
  return $ take length str