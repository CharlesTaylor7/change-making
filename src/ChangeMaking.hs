module ChangeMaking where

-- base
import Control.Arrow ((|||), (&&&))
import Control.Monad.State.Strict (execState, modify)

import Data.Foldable (for_)
import Data.Function (on, (&))
import Data.Monoid(Sum(..),(<>))

-- containers package
import Data.Map (Map, (!?))
import qualified Data.Map as M

-- data types
newtype Currency = Currency { unCurrency :: [Coin] }

newtype Coin = Coin { unCoin :: Int }
  deriving newtype (Eq, Ord)

newtype Money = Money { unMoney :: Int }
  deriving newtype (Eq, Ord, Num)

newtype Count = Count { unCount :: Int }
  deriving newtype (Num, Eq, Ord)

newtype Change = Change { unChange :: [Coin] }

newtype SolutionSet = SolutionSet { unSolutionSet :: Map Money Change }

-- eq & ord
instance Eq Change where
  (==) = (==) `on` coinCount

instance Ord Change where
  compare = compare `on` coinCount

-- Monoid instances
deriving newtype instance Semigroup Change
deriving newtype instance Monoid Change

deriving via (Sum Int) instance Semigroup Count
deriving via (Sum Int) instance Monoid Count

deriving via (Sum Int) instance Semigroup Money
deriving via (Sum Int) instance Monoid Money

instance Semigroup SolutionSet where
  SolutionSet l <> SolutionSet r =
    SolutionSet $
    flip execState mempty $
    for_ (M.toList l) $ \pairL ->
    for_ (M.toList r) $ \pairR -> do
    let (money, change) = pairL <> pairR
    modify $ M.insertWith min money change

instance Monoid SolutionSet where
  mempty = SolutionSet [(Money 0, Change [])]

coinCount :: Change -> Int
coinCount = length . unChange

coinToChange :: Coin -> Change
coinToChange = Change . pure

coinToMoney :: Coin -> Money
coinToMoney = Money . unCoin

-- A functional while loop
-- A `Left a` value tells the loop to continue with `a` as the next argument to `act`.
-- A `Right b`  value signals to exit the loop with b as the result.
loop :: (a -> Either a b) -> a -> b
loop act x = act x & loop act ||| id

solve :: Currency -> Money -> Change
solve (Currency coins) money =
  let
    oneCoinSolutions =
      coins
      & map (coinToMoney &&& coinToChange)
      & M.fromList
      & SolutionSet
  in
    flip loop mempty \solutions ->
      case unSolutionSet solutions !? money of
        Just change -> Right change
        Nothing -> Left $ solutions <> oneCoinSolutions

makeChangeWith :: Currency -> Money -> [Coin]
makeChangeWith = (unChange . ) . solve

makeChange :: Money -> [Coin]
makeChange = makeChangeWith coinsUSA

dollar = Coin 100
quarter = Coin 25
dime = Coin 10
nickel = Coin 5
penny = Coin 1

coinsUSA = Currency [ quarter, dime, nickel, penny]

-- Show instances, purely for debugging
instance Show Change where
  show = show . coinCount

instance Show Coin where
  show (Coin c) = show c <> "-coin"

instance Show Money where
  show (Money m) = show m <> "¢"

instance Show SolutionSet where
  show (SolutionSet conv) =
    let
      pairs =
        M.toList conv
        & map (\(money, change) -> show money <> ": " <> show (coinCount change) <> " coins")
        & unlines
    in
      pairs
--
