module ChangeMaking.Types where

newtype Currency = Currency { unCurrency :: [Coin] }

newtype Coin = Coin { unCoin :: Int }
  deriving (Eq, Ord)

newtype Money = Money { unMoney :: Int }
  deriving (Eq, Ord)
