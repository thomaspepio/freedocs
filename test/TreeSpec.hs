{-# LANGUAGE OverloadedStrings #-}
module TreeSpec where

import Position
import PositionHelper
import Tree
import TreeHelper
import SpecHelper

spec :: Spec
spec = describe "Tree CRDT spec" $ do

    -- TODO : properties about commutativity much ??!!

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

        describe "insertion between bounds that have descendants" $ do
            -- First, a reminder, this code is based on : https://pages.lip6.fr/Marc.Shapiro/papers/icdcs09-treedoc.pdf,
            -- which states that when inserting an atom between positions p1 and p2, you have to find a position p3 such as p1 < p3 < p2,
            -- and insert either to the left of p1, or the right of p2, depending on whichever has no descendants.
            --
            -- Problem is : at some point, you _will_ need to insert an atom between two positions that have descendants.
            --
            -- This test case makes sure that you can insert between nodes that have descendants already, which are called "major nodes" in the paper.
            -- In the example bellow, if we try to insert 'z' between 'b' and 'd', we can see that they both have descendants,
            -- and upon insertion the result should be :
            --
            --  Before insertion :                      After insertion :
            --    |       +--- d ---+                      |       +------- d -------+
            --    |    +- b -+   +- f -+                   |  +- [b,z] -+         +- f -+
            --    |    a     c   e     g                   |  a         c         e     g
            --
            --
            --  In terms of text, these two data structure are respectively (we use [] to mark a point in the text where there is concurrence in editing) :
            --    Before: "abcdefg"
            --    After : "a[bz]cdefg"
            it "should insert between nodes that have descendants" $ do
                let tree_withNoMajorNodes = Branch [Node "d" ""]
                                                (Branch [Node "b" ""]
                                                    (Branch [Node "a" ""] Empty Empty)
                                                    (Branch [Node "c" ""] Empty Empty))
                                                (Branch [Node "f" ""]
                                                    (Branch [Node "e" ""] Empty Empty)
                                                    (Branch [Node "g" ""] Empty Empty))
                
                let tree_AfterInsertion = Branch [Node "d" ""]
                                                (Branch [Node "b" "", Node "z" ""]
                                                    (Branch [Node "a" ""] Empty Empty)
                                                    (Branch [Node "c" ""] Empty Empty))
                                                (Branch [Node "f" ""]
                                                    (Branch [Node "e" ""] Empty Empty)
                                                    (Branch [Node "g" ""] Empty Empty))

                insert (Node "z" "") [Zero] [] tree_withNoMajorNodes `shouldBe` tree_AfterInsertion


            -- The test cases also illustrates what should happen when inserting between nodes that are both already major nodes.
            -- So again, if we try to insert 'z' between 'x' and 'd', we're concerned with which node will welcome the new atom,
            -- and in an effort to balance things, we choose the one that has the lowest atom count :
            -- 
            -- Before insertion :                       After insertion :          
            --    |        +----- [d, y] -----+            |        +----- [z,d,y] -----+
            --    |  +- [b,w,x] -+         +- f -+         |  +- [b,w,x] -+          +- f -+
            --    |  a           c         e      g        |  a           c          e      g
            --
            --  In terms of text, these two data structure are respectively (we use [] to mark a point in the text where there is concurrence in editing) :
            --      Before : "a[bwx]c[dy]efg"
            --      After  : "a[bwx]c[zdy]efg"
            it "should insert between nodes that have descendants" $ do
                let tree_withNoMajorNodes = Branch [Node "d" "", Node "y" ""]
                                                (Branch [Node "b" "", Node "w" "", Node "x" ""]
                                                    (Branch [Node "a" ""] Empty Empty)
                                                    (Branch [Node "c" ""] Empty Empty))
                                                (Branch [Node "f" ""]
                                                    (Branch [Node "e" ""] Empty Empty)
                                                    (Branch [Node "g" ""] Empty Empty))
                
                let tree_AfterInsertion = Branch [Node "z" "", Node "d" "", Node "y" ""]
                                                (Branch [Node "b" "", Node "w" "", Node "x" ""]
                                                    (Branch [Node "a" ""] Empty Empty)
                                                    (Branch [Node "c" ""] Empty Empty))
                                                (Branch [Node "f" ""]
                                                    (Branch [Node "e" ""] Empty Empty)
                                                    (Branch [Node "g" ""] Empty Empty))

                insert (Node "z" "") [Zero] [] tree_withNoMajorNodes `shouldBe` tree_AfterInsertion