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
        (False, _) -> insertAt node (lower ++ [One]) tree
        (_, False) -> insertAt node (upper ++ [Zero]) tree
        _          -> insertAt node lower tree -- TODO : hmm sure about that one ?
    | otherwise             = tree

insertAt :: Node -> Position -> Tree -> Tree
insertAt node _ Empty                                = newTree node
insertAt node [] (Branch content left right)         = Branch (node : content) left right
insertAt node (Zero:[]) (Branch content Empty Empty) = Branch content (newTree node) Empty
insertAt node (One:[]) (Branch content Empty Empty)  = Branch content Empty (newTree node)
insertAt node (Zero:ids) (Branch content left right) = Branch content (insertAt node ids left) right
insertAt node (One:ids) (Branch content left right)  = Branch content left (insertAt node ids right)

newTree :: Node -> Tree
newTree node = Branch [node] Empty Empty

hasDescendants :: Position -> Tree -> Bool
hasDescendants _ Empty                         = False
hasDescendants (Zero:[]) (Branch _ Empty _)    = False
hasDescendants (One:[]) (Branch _ _ Empty)     = False
hasDescendants (One:[]) (Branch _ left right)  = True
hasDescendants (Zero:[]) (Branch _ left right) = True
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
