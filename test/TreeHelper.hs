module TreeHelper(
    genNode, 
    genEmptyTree, genNonEmptyTree, genTreeWithNoChilds,
    leftmostPosition, rightmostPosition, deepestPosition
) where

import qualified Data.Text as T
import           Position
import           PositionHelper
import           Tree   
import           SpecHelper

-- | Generator for a non empty tree of a choosen depth
genNonEmptyTree :: Int -> Gen Tree
genNonEmptyTree depth
    | depth == 1 = do
        node <- genNode
        left <- genTreeWithNoChilds
        right <- genTreeWithNoChilds
        return $ Branch [node] left right
    | otherwise = do
        node <- genNode
        left <- oneof [genNonEmptyTree (depth - 1), genEmptyTree]
        right <- oneof [genNonEmptyTree (depth - 1), genEmptyTree]
        return $ Branch [node] left right

-- | Generator for a tree that has no child (a tree of a single node)
genTreeWithNoChilds :: Gen Tree
genTreeWithNoChilds = do
    node <- genNode
    return $ Branch [node] Empty Empty

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

leftmostPosition :: Tree -> Position
leftmostPosition Empty             = [] 
leftmostPosition (Branch _ left _) = Zero : leftmostPosition left

rightmostPosition :: Tree -> Position
rightmostPosition Empty              = []
rightmostPosition (Branch _ _ right) = One : rightmostPosition right

deepestPosition :: Tree -> Position
deepestPosition Empty = []
deepestPosition (Branch _ Empty right) = One : deepestPosition right
deepestPosition (Branch _ left Empty)  = Zero : deepestPosition left
deepestPosition (Branch _ left right)  = 
    let 
        deepestLeft = Zero : deepestPosition left
        deepestRight = One : deepestPosition right
    in
        if (length deepestLeft) <= (length deepestRight) then deepestRight else deepestLeft