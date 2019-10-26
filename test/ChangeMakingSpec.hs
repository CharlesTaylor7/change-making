module ChangeMakingSpec where

import Data.Function ((&))

import Data.Map (Map)
import qualified Data.Map as M

import Test.Hspec

import ChangeMaking

spec = do
  describe "ChangeMaking" $ do
    describe "makeChangeWith" $ do
      it "works" $ do
        sequenceA testCases
        pure ()

testCases :: [IO ()]
testCases =
  [
    makeChange (Money 1) `shouldBe` [penny],
    makeChange (Money 25) `shouldBe` [quarter],
    makeChange (Money 40) `shouldBe` [quarter, dime, nickel],
    makeChange (Money 20) `shouldBe` [dime, dime],
    makeChange (Money 100) `shouldBe` [quarter, quarter, quarter, quarter],
    makeChange (Money 73) `shouldBe` [quarter, quarter, dime, dime, penny, penny, penny],
    let
      coinSet = Currency $ Coin <$> [50, 20, 15, 5, 4, 1]
      expected = Coin <$> [15, 4, 4]
    in
      makeChangeWith coinSet (Money 23) `shouldBe` expected,
    let
      coinSet = Currency $ Coin <$> [1, 5, 10, 21, 25]
      expected = Coin <$> [21, 21, 21]
    in
      makeChangeWith coinSet (Money 63) `shouldBe` expected,
      pure ()
  ]
