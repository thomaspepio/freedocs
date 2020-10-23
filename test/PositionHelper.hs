module PositionHelper(
    genIdentifier,
    genPosition, genOrderedPositions, genPositionBefore, genPositionAfter, 
) where

import Position
import SpecHelper

genPosition :: Gen Position
genPosition = sized $ \n -> do
    length <- choose (1, n)
    vectorOf length genIdentifier

genPositionBefore :: Position -> Gen Position
genPositionBefore position = do
    generated <- genPosition
    return $ position <> [Zero] <> generated

genPositionAfter :: Position -> Gen Position
genPositionAfter position = do
    generated <- genPosition
    return $ position <> [One] <> generated

genOrderedPositions :: Gen (Position, Position)
genOrderedPositions = do
    position <- genPosition
    before <- genPositionBefore position
    after <- genPositionAfter position
    oneof [genPair before position
         , genPair position after]

genIdentifier :: Gen Identifier
genIdentifier = oneof [return Zero, return One]

instance Arbitrary Identifier where arbitrary = genIdentifier