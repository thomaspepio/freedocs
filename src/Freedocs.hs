{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE GADTs             #-}
module Freedocs(
    User, Atom, 
    Node(..), Tree(..),
    Identifier(..), Position,
    insert
) where

import qualified Data.Text as T

type User = T.Text
type Atom = T.Text

data Node = Node Atom User
    deriving(Eq, Show)

-- data Empty           TODO : use type labels to label trees
-- data NonEmpty
data Tree where
    Empty  :: Tree
    Branch :: { node :: Node
              , left :: Tree
              , right :: Tree } 
           -> Tree

data Identifier = Zero | One deriving(Eq, Show)
type Position = [Identifier]

insert :: Node -> Position -> Position -> Tree -> Tree
insert node _ _ Empty = Branch node Empty Empty
insert node lower upper tree = undefined -- /!\ PLS IMPLEMENT THIS /!\

instance Show Tree where
    show Empty = ""
    show (Branch node left right) = show node <> "\n  |-" <> show left <> "\n  |-" <> show right

instance Eq Tree where
    Empty == Empty                                          = True
    Empty == _                                              = False
    _ == Empty                                              = False
    (Branch node left right) == (Branch node' left' right') = node == node' && left == left' && right == right'

instance Ord Identifier where
    compare Zero Zero = EQ
    compare One One   = EQ
    compare Zero One  = LT
    compare One Zero  = GT
