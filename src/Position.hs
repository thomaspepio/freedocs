module Position(
    Identifier(..), Position,
    (<?)
) where

data Identifier = Zero | One deriving(Eq, Show)
instance Ord Identifier where
    compare Zero Zero = EQ
    compare One One   = EQ
    compare Zero One  = LT
    compare One Zero  = GT

type Position = [Identifier]

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