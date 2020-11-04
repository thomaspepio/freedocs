{-# LANGUAGE OverloadedStrings #-}
module SpecHelper (
  module Test.Hspec,
  module Test.QuickCheck,
  genChar, genPair, genTriple, genText
) where

import qualified Data.Text as T
import           Test.Hspec
import           Test.QuickCheck

genPair :: a -> b -> Gen (a, b)
genPair left right = pure (left, right)

genTriple :: a -> b -> c -> Gen (a, b, c)
genTriple a b c = pure (a, b, c)

genChar :: Gen Char
genChar = elements ['a'..'z']

genText :: Int -> Gen T.Text
genText length = do
  str <- listOf genChar
  return $ T.pack $ take length str