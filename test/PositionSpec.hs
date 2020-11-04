module PositionSpec where

import Position
import PositionHelper
import SpecHelper

spec :: Spec
spec = describe "Positions spec" $ do

    describe "Properties" $ do
        it "order of positions" $ do
            property $ forAll genOrderedPositions $ \(lower, upper) -> (lower <? upper == True)

        it "any position that starts with zero is lower than the root" $ do
            let prop_LowerThanRoot position = Zero :> position <? root == True
            property prop_LowerThanRoot
        
        it "any position that starts with one is higher than the root" $ do
            let prop_HigherThanRoot position = One :> position <? root == False
            property prop_HigherThanRoot
        
    describe "Examples" $ do
        it "common prefixes" $ do
            commonPrefix (Zero :> (One :> (Zero :> (Last Zero Nothing)))) (Zero :> (One :> (Zero :> (Last One Nothing)))) `shouldBe` [Zero, One, Zero]

        it "almost equality of identifiers" $ do
            (Zero :> (One :> (Last One Nothing))) ~= (Zero :> (One :> (Last One (Just 1)))) `shouldBe` True
        
        it "append identifier to a position" $ do
            (Last None Nothing) ++> Zero `shouldBe` Last Zero Nothing
            (Last None (Just 1)) ++> Zero `shouldBe` Last Zero Nothing
            (Last One Nothing) ++> Zero `shouldBe` One :> (Last Zero Nothing)
            (Last One (Just 1)) ++> Zero `shouldBe` One :> (Last Zero (Just 1))
            Zero :> (Last One Nothing) ++> Zero `shouldBe` Zero :> (One :> (Last Zero Nothing))
            Zero :> (Last One (Just 1)) ++> Zero `shouldBe` Zero :> (One :> (Last Zero (Just 1)))

        describe "Prelude functions with a mustache adapteed for positions" $ do
            let pos = (Zero :> (One :> (Zero :> (Last Zero Nothing))))

            it "len : length of positions" $ do
                len pos `shouldBe` 4
            
            it "gimme : take n elements from a position" $ do
                gimme 1 pos `shouldBe` Last Zero Nothing
                gimme 2 pos `shouldBe` (Zero :> Last One Nothing)
            
            it "leave : drop n elements from a position" $ do
                leave 1 pos `shouldBe` (One :> (Zero :> (Last Zero Nothing)))
                leave 2 pos `shouldBe` (Zero :> (Last Zero Nothing))
                leave 5 pos `shouldBe` root
            
            it "headOf : first identifier of a position" $ do
                headOf pos `shouldBe` Zero
                headOf (Last Zero Nothing) `shouldBe` Zero
                headOf (Last Zero (Just 1)) `shouldBe` Zero