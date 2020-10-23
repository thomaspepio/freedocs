{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE GADTs             #-}
module Freedocs(
    User, Atom, 
    Node(..), Tree(..),
    Identifier(..), Position,
    insert, hasDescendants, (<?),
    leftmostPosition, rightmostPosition, deepestPosition, countNodes
) where

import qualified Data.Text as T

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

data Identifier = Zero | One deriving(Eq, Show)
instance Ord Identifier where
    compare Zero Zero = EQ
    compare One One   = EQ
    compare Zero One  = LT
    compare One Zero  = GT

type Position = [Identifier]

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

newTree :: Node -> Tree
newTree node = Branch [node] Empty Empty

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

(<?) :: Position -> Position -> Bool
(<?) [Zero] [One] = True
(<?) left right   = case (left `isPrefix` right, right `isPrefix` left) of
    (True, _) -> head (drop (length left) right) == One
    (_, True) -> head (drop (length right) left) == Zero
    (_, _)    -> False
infixr 5 <?

isPrefix :: Position -> Position -> Bool
isPrefix [Zero] [One] = True
isPrefix left right   = take (length left) right == left

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

countNodes :: Tree -> Int
countNodes Empty                 = 0
countNodes (Branch nodes left right) = length nodes + (countNodes left) + (countNodes right)

get :: Position -> Tree -> Tree
get _ Empty                      = Empty
get [] tree                      = tree
get (One:[]) (Branch _ _ right)  = right
get (Zero:[]) (Branch _ left _)  = left
get (One:ids) (Branch _ _ right) = get ids right
get (Zero:ids) (Branch _ left _) = get ids left