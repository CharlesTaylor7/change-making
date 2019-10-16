module ChangeMakingSpec where

import ChangeMaking
import Test.QuickCheck
import Test.Hspec

import Data.Map (Map)
import qualified Data.Map as M

import Data.Monoid
import Data.Traversable
import Data.Bifunctor
import Control.Lens ((&))

spec = do
  describe "ChangeMaking" $ do
    describe "makeChangeWith" $ do
      it "works" $ do
        sequenceA testCases
        pure ()

testCases :: [IO ()]
testCases =
  [
    makeChange 1 `shouldBe` [penny],
    makeChange 25 `shouldBe` [quarter],
    makeChange 40 `shouldBe` [quarter, dime, nickel],
    makeChange 20 `shouldBe` [dime, dime],
    makeChange 100 `shouldBe` [quarter, quarter, quarter, quarter],
    makeChange 73 `shouldBe` [quarter, quarter, dime, dime, penny, penny, penny],
    let
      coinSet = Currency $ Coin <$> [50, 20, 15, 5, 1]
      expected = Coin <$> [15, 4, 4]
    in
      makeChangeWith coinSet 23 `shouldBe` expected,
    let
      coinSet = Currency $ Coin <$> [1, 5, 10, 21, 25]
      expected = Coin <$> [21, 21, 21]
    in
      makeChangeWith coinSet 63 `shouldBe` expected,
      pure ()
  ]
