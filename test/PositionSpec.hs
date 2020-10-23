module PositionSpec where

import Position
import PositionHelper
import SpecHelper

spec :: Spec
spec = describe "Positions spec" $ do

    describe "identifiers and positions" $ do
        it "order of positions" $ do
            let prop_Prefixes = forAll genOrderedPositions $ \(lower, upper) -> lower <? upper == True
            quickCheck prop_Prefixes
