module FreedocsGenerators(
    genTree, genNonEmptyTree, genTreeWithNoChilds, genEmptyTree,
    genNode, genPosition, genIdentifier
) where

import Data.Text
import Freedocs
import SpecHelper

genTree :: Int -> Gen Tree
genTree depth = oneof [genNonEmptyTree depth, genEmptyTree]

genNonEmptyTree :: Int -> Gen Tree
genNonEmptyTree depth 
    | depth == 0 = do
        node <- genNode
        left <- genTreeWithNoChilds
        right <- genTreeWithNoChilds
        return $ Branch node left right
    | otherwise = do
        node <- genNode
        left <- oneof [genNonEmptyTree (depth - 1), genEmptyTree]
        right <- oneof [genNonEmptyTree (depth - 1), genEmptyTree]
        return $ Branch node left right

genTreeWithNoChilds :: Gen Tree
genTreeWithNoChilds = do
    node <- genNode
    return $ Branch node Empty Empty

genEmptyTree :: Gen Tree
genEmptyTree = pure Empty

instance Arbitrary Tree where
    arbitrary = do
        depth <- choose (1, 5)
        genTree depth

genNode :: Gen Node
genNode = do
    atom <- genString
    user <- genString
    return $ Node (pack atom) (pack user)

instance Arbitrary Node where
    arbitrary = genNode

genPosition :: Gen Position
genPosition = listOf genIdentifier

genIdentifier :: Gen Identifier
genIdentifier = oneof [return Zero, return One]

instance Arbitrary Identifier where
    arbitrary = genIdentifier