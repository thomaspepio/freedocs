module FreedocsHelpers(
    countNodes, deepestPosition,
    genNonEmptyTree, genTreeWithNoChilds, genEmptyTree,
    genNode, genPosition, genOrderedPositions, genPositionBefore, genPositionAfter, 
    genIdentifier
) where

import qualified Data.Text as T
import Freedocs
import SpecHelper

-- | Generator for a non empty tree of a choosen depth
genNonEmptyTree :: Int -> Gen Tree
genNonEmptyTree depth
    | depth == 1 = do
        node <- genNode
        left <- genTreeWithNoChilds
        right <- genTreeWithNoChilds
        return $ Branch node left right
    | otherwise = do
        node <- genNode
        left <- oneof [genNonEmptyTree (depth - 1), genEmptyTree]
        right <- oneof [genNonEmptyTree (depth - 1), genEmptyTree]
        return $ Branch node left right

-- | Generator for a tree that has no child (a tree of a single node)
genTreeWithNoChilds :: Gen Tree
genTreeWithNoChilds = do
    node <- genNode
    return $ Branch node Empty Empty

-- | Generator for empty trees
genEmptyTree :: Gen Tree
genEmptyTree = pure Empty

instance Arbitrary Tree where
    arbitrary = do
        depth <- choose (1, 5)
        genNonEmptyTree depth

-- | Generator for nodes, with atom and user fixed to length = 5
genNode :: Gen Node
genNode = do
    atom <- genString 5
    user <- genString 5
    return $ Node (T.pack atom) (T.pack user)

instance Arbitrary Node where arbitrary = genNode

genPosition :: Gen Position
genPosition = sized $ \n -> do
    length <- choose (1, n)
    vectorOf length genIdentifier

genPositionBefore :: Position -> Gen Position
genPositionBefore position = do
    generated <- genPosition
    return $ position <> [Zero] <> generated

genPositionAfter :: Position -> Gen Position
genPositionAfter position = do
    generated <- genPosition
    return $ position <> [One] <> generated

genOrderedPositions :: Gen (Position, Position)
genOrderedPositions = do
    position <- genPosition
    before <- genPositionBefore position
    after <- genPositionAfter position
    oneof [genPair before position
         , genPair position after]

genIdentifier :: Gen Identifier
genIdentifier = oneof [return Zero, return One]

instance Arbitrary Identifier where arbitrary = genIdentifier