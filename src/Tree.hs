{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE GADTs             #-}
module Tree(
    Atom, Node(..), Tree(..), User, 
    countNodes, hasDescendants, insert
) where

import qualified Data.Text as T
import           Position

type User = T.Text
type Atom = T.Text

data Node = Node Atom User
    deriving(Eq, Show)

--data Empty    TODO : for later, if we decide/need to tag trees
--data NonEmpty
data Tree where
    Empty  :: Tree
    Branch :: { nodes :: [Node]
              , left :: Tree
              , right :: Tree } 
              -> Tree
    deriving(Eq, Show)

insert :: Node -> Position -> Position -> Tree -> Tree
insert _ _ _ Empty = Empty
insert node lower upper tree
    | lower <? upper = case (hasDescendants lower tree, hasDescendants upper tree) of
        (False, _) -> newNodeAt node (lower ++ [One]) tree
        (_, False) -> newNodeAt node (upper ++ [Zero]) tree

        _          -> let nbNodesAtLower = length . nodes $ get lower tree
                          nbNodesAtUpper = length . nodes $ get upper tree
                      in if(nbNodesAtLower <= nbNodesAtUpper)
                            then insertAtExistingNode node lower ToTheLeft tree
                            else insertAtExistingNode node upper ToTheRight tree
    
    | otherwise             = tree

newNodeAt :: Node -> Position -> Tree -> Tree
newNodeAt node _ Empty                                = newTree node
newNodeAt node (Zero:[]) (Branch content Empty Empty) = Branch content (newTree node) Empty
newNodeAt node (One:[]) (Branch content Empty Empty)  = Branch content Empty (newTree node)
newNodeAt node (Zero:ids) (Branch content left right) = Branch content (newNodeAt node ids left) right
newNodeAt node (One:ids) (Branch content left right)  = Branch content left (newNodeAt node ids right)

data DirectionForInsert = ToTheLeft | ToTheRight
insertAtExistingNode :: Node -> Position -> DirectionForInsert -> Tree -> Tree
insertAtExistingNode _ _ _ Empty                                           = Empty
insertAtExistingNode node [] ToTheLeft (Branch content left right)         = Branch (content ++ [node]) left right
insertAtExistingNode node [] ToTheRight (Branch content left right)        = Branch (node : content) left right
insertAtExistingNode node (Zero:ids) direction (Branch content left right) = Branch content (insertAtExistingNode node ids direction left) right
insertAtExistingNode node (One:ids) direction (Branch content left right)  = Branch content left (insertAtExistingNode node ids direction right)

hasDescendants :: Position -> Tree -> Bool
hasDescendants _ Empty                         = False
hasDescendants [] (Branch _ Empty Empty)       = False
hasDescendants [] _                            = True
hasDescendants (_:[]) (Branch _ Empty Empty)   = False
hasDescendants (Zero:[]) (Branch _ Empty _)    = False
hasDescendants (One:[]) (Branch _ _ Empty)     = False
hasDescendants (id:ids) (Branch _ left right)
    | id == Zero = hasDescendants ids left
    | id == One  = hasDescendants ids right

newTree :: Node -> Tree
newTree node = Branch [node] Empty Empty

get :: Position -> Tree -> Tree
get _ Empty                      = Empty
get [] tree                      = tree
get (One:[]) (Branch _ _ right)  = right
get (Zero:[]) (Branch _ left _)  = left
get (One:ids) (Branch _ _ right) = get ids right
get (Zero:ids) (Branch _ left _) = get ids left

countNodes :: Tree -> Int
countNodes Empty                 = 0
countNodes (Branch nodes left right) = length nodes + (countNodes left) + (countNodes right)