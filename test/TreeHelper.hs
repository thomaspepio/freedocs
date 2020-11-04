{-# LANGUAGE OverloadedStrings #-} 
module TreeHelper(
    genAtom, 
    genEmptyTree, genNonEmptyTree, genNonEmptyTreeWithConsecutivePositions, genTreeWithNoChilds,
    leftmostPosition, rightmostPosition, deepestPosition
) where

import qualified Data.Text as T
import           Position
import           Tree   
import           SpecHelper

-- | Generator for a non empty tree of a choosen depth
genNonEmptyTree :: Int -> Gen Tree
genNonEmptyTree depth
    | depth == 1 = do
        node <- genAtom
        return $ Branch (Left node) Empty Empty
    | depth == 2 = do
        node <- genAtom
        left <- genTreeWithNoChilds
        right <- genTreeWithNoChilds
        return $ Branch (Left node) left right
    | otherwise = do
        node <- genAtom
        left <- oneof [genNonEmptyTree (depth - 1), genEmptyTree]
        right <- oneof [genNonEmptyTree (depth - 1), genEmptyTree]
        return $ Branch (Left node) left right

genNonEmptyTreeWithConsecutivePositions :: Gen (Tree, Position, Position)
genNonEmptyTreeWithConsecutivePositions = sized $ \n -> do
    tree <- genNonEmptyTree (n + 3)
    let leftmost = leftmostPosition tree
    let leftMostMinus = minus 1 leftmost
    let rightmost = rightmostPosition tree
    let rightmostMinus = minus 1 rightmost
    
    oneof [genTriple tree leftmost leftMostMinus
          , genTriple tree rightmostMinus rightmost]

-- | Generator for a tree that has no child (a tree of a single node)
genTreeWithNoChilds :: Gen Tree
genTreeWithNoChilds = do
    node <- genAtom
    return $ Branch (Left node) Empty Empty

-- | Generator for empty trees
genEmptyTree :: Gen Tree
genEmptyTree = pure Empty

instance Arbitrary Tree where
    arbitrary = do
        depth <- choose (1, 5)
        genNonEmptyTree depth

-- | Generator for nodes, with atom and user fixed to length = 5
genAtom :: Gen Atom
genAtom = genText 5

instance Arbitrary T.Text where arbitrary = genAtom

leftmostPosition :: Tree -> Position
leftmostPosition Empty              = Last None Nothing
leftmostPosition (Branch _ Empty _) = Last Zero Nothing
leftmostPosition (Branch _ left _)  = Zero :> leftmostPosition left

rightmostPosition :: Tree -> Position
rightmostPosition Empty              = Last None Nothing
rightmostPosition (Branch _ _ Empty) = Last One Nothing
rightmostPosition (Branch _ _ right) = One :> rightmostPosition right

deepestPosition :: Tree -> Position
deepestPosition Empty = Last None Nothing
deepestPosition (Branch _ Empty right) = One :> deepestPosition right
deepestPosition (Branch _ left Empty)  = Zero :> deepestPosition left
deepestPosition (Branch _ left right)  = 
    let 
        deepestLeft = Zero :> deepestPosition left
        deepestRight = One :> deepestPosition right
    in
        if (len deepestLeft) <= (len deepestRight) then deepestRight else deepestLeft

minus :: Int -> Position -> Position
minus 0 pos                = pos
minus _ (Last _ _)         = Last None Nothing
minus _ (id :> (Last _ _)) = Last id Nothing
minus x (_ :> ids)         = minus (x - 1) ids