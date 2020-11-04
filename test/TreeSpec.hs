{-# LANGUAGE OverloadedStrings #-}
module TreeSpec where

import Position
import PositionHelper
import Tree
import TreeHelper
import SpecHelper

spec :: Spec
spec = describe "Tree CRDT spec" $ do

    describe "Properties" $ do
        describe "descendants"$ do
            it "an empty tree has no descendants" $ do
                let prop_EmptyTree_HasNoDescendants position = forAll genEmptyTree $ \tree -> hasDescendants position tree == Nothing
                quickCheck prop_EmptyTree_HasNoDescendants

            it "the deepest leaf of a non empty tree has no descendants" $ do
                let prop_NonEmptyTree_HasDescendants = forAll (genNonEmptyTree 2) $ \tree -> let deepest = deepestPosition tree in hasDescendants deepest (get deepest tree) == Nothing
                quickCheck prop_NonEmptyTree_HasDescendants

            it "leftmost leaf has no descendants" $ do
                let prop_LeftmostLeafNoDescendants = forAll (genNonEmptyTree 2) $ \tree -> let leftmostPos = leftmostPosition tree in hasDescendants leftmostPos (get leftmostPos tree) == Nothing
                quickCheck prop_LeftmostLeafNoDescendants

            it "rightmost leaf has no descendants" $ do
                let prop_RightmostLeafNoDescendants = forAll (genNonEmptyTree 3) $ \tree -> let rightmostPos = rightmostPosition tree in hasDescendants rightmostPos (get rightmostPos tree) == Nothing
                quickCheck prop_RightmostLeafNoDescendants
        
        describe "insertion of atoms" $ do
            it "two inserts that refer to different positions can happen in any order" $ do
                let positionOne = Last One Nothing
                let positionZero = Last Zero Nothing
                let prop_Commutativity_InsertAtTwoDifferentPositions node = forAll (genNonEmptyTree 3) $ \tree -> insert node root positionOne (insert node positionZero root tree) == insert node positionZero root (insert node root positionOne tree)
                quickCheck prop_Commutativity_InsertAtTwoDifferentPositions
        
            it "insert does nothing if lower and upper bounds are not ordered" $ do
                let prop_InsertWith_UnorderedBounds_DoesNothing atom = forAll (genNonEmptyTree 3) $ \tree -> forAll genOrderedPositions $ \(lower, upper) -> insert atom upper lower tree == tree
                quickCheck prop_InsertWith_UnorderedBounds_DoesNothing

            it "inserting an atom in any empty tree yields an empty tree" $ do
                let prop_InsertEmptyTree atom = forAll genEmptyTree $ \emptyTree -> forAll genOrderedPositions $ \(lower, upper) -> insert atom lower upper emptyTree == Empty
                quickCheck prop_InsertEmptyTree

            it "upon insertion a tree's atom count is incremented" $ do
                let prop_LengthTreeIncerment atom = forAll genNonEmptyTreeWithConsecutivePositions $ \(tree, lower, upper) -> (countNodes (insert atom lower upper tree)) == countNodes tree + 1
                quickCheck prop_LengthTreeIncerment


    describe "Examples" $ do
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
            it "Before = abcdefg / After = a[bz]cdefg" $ do
                let tree_withNoMajorNodes = Branch (Left "d")
                                                (Branch (Left "b")
                                                    (Branch (Left "a") Empty Empty)
                                                    (Branch (Left "c") Empty Empty))
                                                (Branch (Left "f")
                                                    (Branch (Left "e") Empty Empty)
                                                    (Branch (Left "g") Empty Empty))
                
                let tree_AfterInsertion = Branch (Left "d")
                                                (Branch (Right [(0, "b"), (1, "z")])
                                                    (Branch (Left "a") Empty Empty)
                                                    (Branch (Left "c") Empty Empty))
                                                (Branch (Left "f")
                                                    (Branch (Left "e") Empty Empty)
                                                    (Branch (Left "g") Empty Empty))

                insert "z" (Last Zero Nothing) root tree_withNoMajorNodes `shouldBe` tree_AfterInsertion


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
            it "Before = a[bwx]c[dy]efg / After = a[bwx]c[zdy]efg" $ do
                let tree_withNoMajorNodes = Branch (Right [(1, "d"), (2, "y")])
                                                (Branch (Right [(0, "b"), (1, "w"), (2, "x")])
                                                    (Branch (Left "a") Empty Empty)
                                                    (Branch (Left "c") Empty Empty))
                                                (Branch (Left "f")
                                                    (Branch (Left "e") Empty Empty)
                                                    (Branch (Left "g") Empty Empty))
                
                let tree_AfterInsertion = Branch (Right [(0, "z"), (1, "d"), (2, "y")])
                                                (Branch (Right [(0, "b"), (1, "w"), (2, "x")])
                                                    (Branch (Left "a") Empty Empty)
                                                    (Branch (Left "c") Empty Empty))
                                                (Branch (Left "f")
                                                    (Branch (Left "e") Empty Empty)
                                                    (Branch (Left "g") Empty Empty))

                insert "z" (Last Zero Nothing) root tree_withNoMajorNodes `shouldBe` tree_AfterInsertion
