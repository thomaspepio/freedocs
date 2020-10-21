{-# LANGUAGE OverloadedStrings #-}
module FreedocsSpec where

import Freedocs
import FreedocsHelpers
import SpecHelper

spec :: Spec
spec = describe "Freedocs" $ do

    describe "identifiers and positions" $ do
        it "order of positions" $ do
            let prop_Prefixes = forAll genOrderedPositions $ \(lower, upper) -> lower <? upper == True
            quickCheck prop_Prefixes


    describe "ascendants and descendents" $ do
        it "an empty tree has no descendants" $ do
            let prop_EmptyTree_HasNoDescendants position = forAll genEmptyTree $ \tree -> hasDescendants position tree == False
            quickCheck prop_EmptyTree_HasNoDescendants

        it "the deepest leaf of a non empty tree has no descendants" $ do
            let prop_NonEmptyTree_HasDescendants = forAll (genNonEmptyTree 5) $ \tree -> hasDescendants (deepestPosition tree) tree == False
            quickCheck prop_NonEmptyTree_HasDescendants

        it "leftmost leaf has no descendants" $ do
            let prop_LeftmostLeafNoDescendants = forAll (genNonEmptyTree 5) $ \tree -> hasDescendants (leftmostPosition tree) tree == False
            quickCheck prop_LeftmostLeafNoDescendants

        it "rightmost leaf has no descendants" $ do
            let prop_RightmostLeafNoDescendants = forAll (genNonEmptyTree 5) $ \tree -> hasDescendants (rightmostPosition tree) tree == False
            quickCheck prop_RightmostLeafNoDescendants


    describe "insert nodes in a tree" $ do
        it "insert does nothing if lower and upper bounds are not ordered" $ do
            let prop_InsertWith_UnorderedBounds_DoesNothing node = forAll (genNonEmptyTree 5) $ \tree -> forAll genOrderedPositions $ \(lower, upper) -> insert node upper lower tree == tree
            quickCheck prop_InsertWith_UnorderedBounds_DoesNothing

        it "inserting a node in any empty tree yields an empty tree" $ do
            let prop_InsertEmptyTree node = forAll genEmptyTree $ \emptyTree -> forAll genOrderedPositions $ \(lower, upper) -> insert node lower upper emptyTree == Empty
            quickCheck prop_InsertEmptyTree

        it "upon insertion a tree's node count is incremented" $ do
            let prop_LengthTreeIncerment node = forAll (genNonEmptyTree 2) $ \tree -> (countNodes (insert node [Zero] [Zero, One] tree)) == countNodes tree + 1
            quickCheck prop_LengthTreeIncerment
