module ChangeMaking.UtilsSpec where

import ChangeMaking
import ChangeMaking.Utils

import Data.Monoid

import Test.Hspec

spec =
  describe "merge" $ do
    it "combines elements in a monoid; assumes the elements are commutative" $ do
      let list = Sum <$> [1, 2, -3, 8, 102]
      merge list `shouldBe` 110
    it "works for strings; sorta" $ do
      let list = ["My", "name", "is", "Inigo", "Montoya"]
      merge list `shouldBe` "MyInigoisnameMontoya"
