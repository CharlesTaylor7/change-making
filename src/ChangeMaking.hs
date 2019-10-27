module ChangeMaking
  ( makeChange
  , makeChangeWith
  , penny
  , nickel
  , dime
  , quarter
  , Money(..)
  , Coin(..)
  , Currency(..)
  ) where

-- base
import Control.Arrow ((|||), (&&&))

import Data.Foldable (for_)
import Data.Function (on, (&))
import Data.List (sortOn)
import Data.Ord (Down(..))

-- containers package
import Data.Map (Map, (!?))
import qualified Data.Map as Map

-- mtl package
import Control.Monad.State.Strict (execState, modify)

-- data types
newtype Currency = Currency { unCurrency :: [Coin] }

newtype Coin = Coin { unCoin :: Int }
  deriving (Eq, Ord)

newtype Money = Money { unMoney :: Int }
  deriving (Eq, Ord)

newtype Count = Count { unCount :: Int }
  deriving (Eq, Ord)

newtype Change = Change { unChange :: [Coin] }

newtype SolutionSet = SolutionSet { unSolutionSet :: Map Money Change }

advanceSolutions :: Money -> Currency -> SolutionSet -> SolutionSet
advanceSolutions (Money t) (Currency coins) (SolutionSet s) =
  SolutionSet $
  flip execState mempty $
  for_ coins $ \(Coin coin) ->
  for_ (Map.toList s) $ \(Money money, Change change) -> do
  let total = coin + money
  let money' = Money total
  let change' = Change (Coin coin : change)
  if total > t
  then pure ()
  else modify $ Map.insert money' change'

initialSolution =
  SolutionSet $
  Map.insert (Money 0) (Change []) $
  Map.empty

-- functions
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
    ordered = Currency (coins & sortOn Down)
    oneCoinSolutions =
      coins
      & map (coinToMoney &&& coinToChange)
      & Map.fromList
      & SolutionSet
  in
    flip loop initialSolution $ \solutions ->
      case unSolutionSet solutions !? money of
        Just change -> Right change
        Nothing -> Left $ advanceSolutions money ordered solutions

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
        Map.toList conv
        & map (\(money, change) -> show money <> ": " <> show (coinCount change) <> " coins")
        & unlines
    in
      pairs
