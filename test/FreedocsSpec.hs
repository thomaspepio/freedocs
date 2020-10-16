{-# LANGUAGE OverloadedStrings #-}
module FreedocsSpec where

import Freedocs
import FreedocsGenerators
import SpecHelper

spec :: Spec
spec = describe "Freedocs" $ do

    describe "insert nodes in a tree" $ do

        it "should insert any node in an empty tree" $ do
            let prop_InsertEmptyTree node lower upper = forAll genEmptyTree $ \emptyTree -> insert node lower upper emptyTree == Branch node Empty Empty
            quickCheck prop_InsertEmptyTree
