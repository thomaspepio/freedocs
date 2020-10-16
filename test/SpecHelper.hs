module SpecHelper (
  module Test.Hspec,
  module Test.QuickCheck,
  genChar, genString
) where

import Test.Hspec
import Test.QuickCheck


genChar :: Gen Char
genChar = elements ['a'..'z']

genString :: Gen String
genString = listOf genChar