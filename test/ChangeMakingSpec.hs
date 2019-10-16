module ChangeMakingSpec where

import ChangeMaking
import Test.QuickCheck
import Test.Hspec

import Data.Map (Map)
import qualified Data.Map as M

import Data.Traversable
import Data.Bifunctor
import Control.Lens ((&))

spec = do
  describe "ChangeMaking" $ do
    describe "makeChangeWith" $ do
      it "works" $ do
        sequenceA testCases
        pure ()
    describe "Convolution" $ do
      it "has a monoid instance which combines minimal solutions" $ do
        sequenceA tests
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

toWeights :: Convolution -> Map Int Int
toWeights (Convolution con) =
  con
  & M.toList
  & map (bimap unMoney coinCount)
  & M.fromList

tests :: [IO ()]
tests =
  let
    a = coinToConvolution $ Coin 1
    b = coinToConvolution $ Coin 5
    c = coinToConvolution $ Coin 4
  in
    [
      let
        result = toWeights $ a <> b
        expected =
            M.insert 0 0 .
            M.insert 1 1 .
            M.insert 5 1 .
            M.insert 6 2 $
            mempty
      in
        result `shouldBe` expected,
      let
        result = toWeights $ a <> b <> c
        expected =
          M.insert 0 0 .
          M.insert 1 1 .
          M.insert 4 1 .
          M.insert 5 1 .
          M.insert 6 2 .
          M.insert 9 2 .
          M.insert 10 3 $
          mempty
      in
        result `shouldBe` expected
    ]
