module PositionHelper(
    genIdentifier, genDisambiguator, genPosition,
    genOrderedPositions, genPositionBefore, genPositionAfter, genOrderedPositionsWithDisambiguator
) where

import Position
import SpecHelper

genIdentifier :: Gen Identifier
genIdentifier = oneof [return Zero, return One]
instance Arbitrary Identifier where arbitrary = genIdentifier

genDisambiguator :: Gen Disambiguator
genDisambiguator = choose (1, 20)

genPositionWithDepth :: Int -> Gen Position
genPositionWithDepth depth 
    | depth == 1 = do
        id <- genIdentifier
        return $ Last id Nothing
    | otherwise = do
        id <- genIdentifier
        rest <- genPositionWithDepth (depth - 1)
        return $ id :> rest

genPosition :: Gen Position
genPosition = sized $ \n -> do
    length <- choose (1, n+1)
    genPositionWithDepth length
instance Arbitrary Position where arbitrary = genPosition

genPositionBefore :: Position -> Gen Position
genPositionBefore position = do
    generated <- genPosition
    return $ setBefore position generated

genPositionAfter :: Position -> Gen Position
genPositionAfter position = do
    generated <- genPosition
    return $ setAfter position generated

genOrderedPositions :: Gen (Position, Position)
genOrderedPositions = do
    position <- genPosition
    before <- genPositionBefore position
    after <- genPositionAfter position
    oneof [genPair before position
          , genPair position after]

genOrderedPositionsWithDisambiguator :: Gen (Position, Position)
genOrderedPositionsWithDisambiguator = do
    (lower, upper) <- genOrderedPositions
    disambiguator <- genDisambiguator
    return (setDisambiguator disambiguator lower
           , setDisambiguator disambiguator upper)

-- TODO : refactoring now that we have ++> ?
setBefore :: Position -> Position -> Position
setBefore (Last id _) pos = id :> (Zero :> pos)
setBefore (id :> ids) pos = id :> setBefore ids pos

setAfter :: Position -> Position -> Position
setAfter (Last id _) pos = id :> (One :> pos)
setAfter (id :> ids) pos = id :> setAfter ids pos

setDisambiguator :: Disambiguator -> Position -> Position
setDisambiguator disambiguator (Last id _)          = Last id (Just disambiguator) 
setDisambiguator disambiguator (id :> (Last id' _)) = id :> Last id' (Just disambiguator)
setDisambiguator disambiguator (id :> ids)          = id :> setDisambiguator disambiguator ids
