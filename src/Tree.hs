{-# LANGUAGE OverloadedStrings #-}
module Tree(
    Atom, Tree(..), Direction(..),
    countNodes, insert, hasDescendants, get
) where

import qualified Data.List as L
import qualified Data.Text as T
import           Position

type Atom = T.Text

data Tree = Empty
          | Branch { nodes :: Either Atom [(Disambiguator, Atom)]
                   , left :: Tree
                   , right :: Tree}
          deriving(Eq, Show)

data Direction = Lefty | Righty | Both deriving(Eq, Show)

insert :: Atom -> Position -> Position -> Tree -> Tree
insert _ _ _ Empty = Empty
insert atom lower upper tree
    | lower <? upper = case (hasDescendants lower tree, hasDescendants upper tree) of
        (Just Lefty, _) -> insertAt atom (lower ++> One) tree
        (_, Just Righty)  -> insertAt atom (upper ++> Zero) tree

        _          -> let nbNodesAtLower = countNodesAt lower tree
                          nbNodesAtUpper = countNodesAt upper tree
                      in if(nbNodesAtLower <= nbNodesAtUpper)                                                           -- Not happy with this part.
                            then if(lower /= root) then insertAt atom lower tree else insertAtRoot atom Righty tree     -- Feels very procedural.
                            else if(upper /= root) then insertAt atom upper tree else insertAtRoot atom Lefty tree      -- 
    
    | otherwise             = tree

insertAt :: Atom -> Position -> Tree -> Tree
insertAt atom _ Empty                                          = newTree atom
insertAt atom (Last Zero Nothing) (Branch content left right)  = case left of
    Empty         -> Branch content (leaf atom) right
    Branch _ _ _  -> Branch content (insertAfterHighestDisambiguator atom left) right
insertAt atom (Last One Nothing) (Branch content left right)   = case right of
    Empty -> Branch content left (leaf atom)
    Branch _ _ _ -> Branch content left (insertBeforeLowestDisambiguator atom right)
insertAt atom (Last Zero (Just d)) (Branch content left right) = Branch content (insertAtDisambiguator atom d left) right
insertAt atom (Last One (Just d)) (Branch content left right)  = Branch content left (insertAtDisambiguator atom d right)
insertAt atom (Zero :> rest) (Branch content left right)       = Branch content (insertAt atom rest left) right
insertAt atom (One :> rest) (Branch content left right)        = Branch content left (insertAt atom rest right)

insertAtRoot :: Atom -> Direction -> Tree -> Tree
insertAtRoot atom _ Empty     = newTree atom
insertAtRoot atom Lefty tree  = insertBeforeLowestDisambiguator atom tree
insertAtRoot atom Righty tree = insertAfterHighestDisambiguator atom tree

leaf :: Atom -> Tree
leaf atom = Branch (Left atom) Empty Empty

insertAfterHighestDisambiguator :: Atom -> Tree -> Tree
insertAfterHighestDisambiguator atom Empty = insertAtDisambiguator atom 0 Empty
insertAfterHighestDisambiguator atom tree  = let disambiguator = highestDisambiguator tree 
                                             in insertAtDisambiguator atom (disambiguator + 1) tree

insertBeforeLowestDisambiguator :: Atom -> Tree -> Tree
insertBeforeLowestDisambiguator atom Empty = insertAtDisambiguator atom 0 Empty
insertBeforeLowestDisambiguator atom tree  = let disambiguator = lowestDisambiguator tree 
                                             in insertAtDisambiguator atom (disambiguator - 1) tree

insertAtDisambiguator :: Atom -> Disambiguator -> Tree -> Tree
insertAtDisambiguator atom _ Empty                     = leaf atom
insertAtDisambiguator atom d (Branch nodes left right) = Branch (Right sortedAtoms) left right
    where 
        sortedAtoms = case nodes of
            Left atom'   -> sortByDisambiguator [(d, atom), (0, atom')]
            Right atoms' -> sortByDisambiguator ((d, atom) : atoms')

lowestDisambiguator :: Tree -> Disambiguator
lowestDisambiguator Empty                      = 0
lowestDisambiguator (Branch (Left _) _ _)      = 0
lowestDisambiguator (Branch (Right atoms) _ _) = fst . head . sortByDisambiguator $ atoms

highestDisambiguator :: Tree -> Disambiguator
highestDisambiguator Empty                      = 0
highestDisambiguator (Branch (Left _) _ _)      = 0
highestDisambiguator (Branch (Right atoms) _ _) = fst . last . sortByDisambiguator $ atoms

sortByDisambiguator :: [(Disambiguator, Atom)] -> [(Disambiguator, Atom)]
sortByDisambiguator atoms = let byDisambiguator (d, _) (d', _) = compare d d' in L.sortBy byDisambiguator atoms

hasDescendants :: Position -> Tree -> Maybe Direction
hasDescendants pos tree = case get pos tree of
    Empty                -> Nothing
    Branch _ Empty Empty -> Nothing
    Branch _ _ Empty     -> Just Lefty
    Branch _ Empty _     -> Just Righty
    Branch _ _ _         -> Just Both

newTree :: Atom -> Tree
newTree atom = Branch (Left atom) Empty Empty

get :: Position -> Tree -> Tree
get _ Empty                               = Empty
get (Last None _) tree                    = tree
get (Last One Nothing) (Branch _ _ right) = right
get (Last Zero Nothing) (Branch _ left _) = left
get (One :> rest) (Branch _ _ right)      = get rest right
get (Zero :> rest) (Branch _ left _)      = get rest left

countNodes :: Tree -> Int
countNodes Empty                             = 0
countNodes (Branch (Left _) left right)      = 1 + (countNodes left) + (countNodes right)
countNodes (Branch (Right nodes) left right) = length nodes + (countNodes left) + (countNodes right)

countNodesAt :: Position -> Tree -> Int
countNodesAt position tree = case get position tree of
    Empty                    -> 0
    Branch (Left _) _ _      -> 1
    Branch (Right nodes) _ _ -> length nodes
