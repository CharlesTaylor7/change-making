module ChangeMakingSpec where

import Test.Hspec

import ChangeMaking

spec = do
  describe "ChangeMaking" $ do
    describe "makeChange: uses standard usa coins" $ do
      it "makes change for a penny" $ do
        makeChange (Money 1) `shouldBe` [penny]
      it "makes change for a quarter" $ do
        makeChange (Money 25) `shouldBe` [quarter]
      it "makes change for 40 cents" $ do
        makeChange (Money 40) `shouldBe` [quarter, dime, nickel]
      it "makes change for 20 cents" $ do
        makeChange (Money 20) `shouldBe` [dime, dime]
      it "makes change for 100 cents" $ do
        makeChange (Money 100) `shouldBe` [quarter, quarter, quarter, quarter]
      it "makes change for 73 cents" $ do
        makeChange (Money 73) `shouldBe` [quarter, quarter, dime, dime, penny, penny, penny]

    describe "makeChangeWith: allows custome currency" $ do
      it "makes change for 23 cents" $ do
        let coinSet = Currency $ Coin <$> [50, 20, 15, 5, 4, 1]
        let expected = Coin <$> [15, 4, 4]
        makeChangeWith coinSet (Money 23) `shouldBe` expected
      it "makes change for 63 cents" $ do
        let coinSet = Currency $ Coin <$> [1, 5, 10, 21, 25]
        let expected = Coin <$> [21, 21, 21]
        makeChangeWith coinSet (Money 63) `shouldBe` expected
