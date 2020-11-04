{-# LANGUAGE OverloadedStrings #-} 
module Position(
    Disambiguator, Identifier(..), Position(..),
    root, (<?), (++>),
    isPrefix, commonPrefix, (~=),
    len, gimme, leave, headOf
) where

import Prelude hiding(id)

type Disambiguator = Int

data Identifier = Zero | One | None deriving(Eq, Show)
instance Ord Identifier where
    compare Zero Zero = EQ
    compare None None = EQ
    compare One One   = EQ
    compare Zero One  = LT
    compare Zero None = LT
    compare One Zero  = GT
    compare One None  = GT
    compare None Zero = GT
    compare None One  = LT

data Position = (:>) Identifier Position 
              | Last Identifier (Maybe Disambiguator)
               deriving(Eq)

(++>) :: Position -> Identifier -> Position
(++>) (Last None _) id = Last id Nothing
(++>) (Last id d) id'  = id :> Last id' d
(++>) (id :> ids) id'  = id :> (ids ++> id')
infixr 5 ++>

root :: Position
root = Last None Nothing

instance Show Position where
    show (id :> rest)       = show id <> " :> " <> show rest
    show (Last id Nothing)  = show id
    show (Last id (Just d)) = show id <> "_" <> show d

(<?) :: Position -> Position -> Bool
(<?) (Last id Nothing) (Last id' Nothing)    = id < id'
(<?) (Last id (Just d)) (Last id' (Just d')) = if id < id' then True else d < d'
(<?) left right
    | right == root         = headOf left == Zero
    | left == root          = headOf right == One
    | left `isPrefix` right = headOf (leave (len left) right) == One
    | right `isPrefix` left = headOf (leave (len right) left) == Zero
    | otherwise             = let prefixLength = length $ commonPrefix left right
                                  leftWithoutPrefix = gimme prefixLength left
                                  rightWithoutPrefix = leave prefixLength right
                              in headOf (leftWithoutPrefix) < headOf (rightWithoutPrefix)
infixr 5 <?

isPrefix :: Position -> Position -> Bool
isPrefix (Last Zero _) (Last One _) = True
isPrefix (Last One _) (Last Zero _) = False
isPrefix left right                 = gimme (len left) right ~= left

commonPrefix :: Position -> Position -> [Identifier]
commonPrefix (Last id _) (Last id' _)  = if id == id' then [id] else []
commonPrefix (Last id _) (id' :> _)    = if id == id' then [id] else []
commonPrefix (id :> _) (Last id' _)    = if id == id' then [id] else []
commonPrefix (id :> ids) (id' :> ids') = if id == id' then id : commonPrefix ids ids' else []

-- | "Almost" equality : two positions are almost equal if they have the same sequence of Zeros | Ones
(~=) :: Position -> Position -> Bool
(~=) (Last One _) (Last One _)      = True
(~=) (Last Zero _) (Last Zero _)    = True
(~=) (One :> rest) (One :> rest')   = True && rest ~= rest'
(~=) (Zero :> rest) (Zero :> rest') = True && rest ~= rest'
(~=) _ _                            = False
infixr 5 ~=

len :: Position -> Int
len (Last _ _)  = 1
len (_ :> rest) = 1 + len rest

gimme :: Int -> Position -> Position
gimme x position
   | x >= len position = position
   | x == 1            = case position of
       Last id _ -> Last id Nothing
       (id :> _) -> Last id Nothing
   | otherwise         = case position of
       Last id d    -> Last id d
       (id :> rest) -> id :> gimme (x - 1) rest

leave :: Int -> Position -> Position
leave 0 position = position
leave x position = case position of
    (_ :> rest)  -> leave (x - 1) rest
    _            -> root

headOf :: Position -> Identifier
headOf (id :> _)   = id
headOf (Last id _) = id
