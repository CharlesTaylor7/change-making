
module ChangeMaking where

import Data.Bifunctor
import Data.Foldable
import Data.Function
import Data.List
import Data.Monoid
import Data.Ord
import Data.Traversable
import Data.Typeable

import Debug.Trace

import Control.Arrow
import Control.Monad.State

-- containers
import Data.Map (Map)
import qualified Data.Map as M

-- data types
newtype Currency = Currency { unCurrency :: [Coin] }

newtype Coin = Coin { unCoin :: Int }
  deriving newtype (Eq, Ord)

newtype Money = Money { unMoney :: Int }
  deriving newtype (Eq, Ord)

newtype Count = Count { unCount :: Int }
  deriving newtype (Num, Eq, Ord)

newtype Change = Change { unChange :: Map Coin Count }

newtype Convolution = Convolution { unConvolution :: Map Money Change }

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

instance Semigroup Convolution where
  Convolution l <> Convolution r =
    Convolution $
    flip execState mempty $
    for_ (M.toList l) $ \pairL ->
    for_ (M.toList r) $ \pairR -> do
    let (money, change) = pairL <> pairR
    modify $ M.insertWith min money change

instance Monoid Convolution where
  mempty = Convolution $ mempty

-- Show instances
instance Show Change where
  show = show . coinCount

instance Show Coin where
  show (Coin c) = show c <> "-coin"

instance Show Money where
  show (Money m) = show m <> "¢"

instance Show Convolution where
  show (Convolution conv) =
    let
      pairs =
        M.toList conv
        & map (\(money, change) -> show money <> ": " <> show (coinCount change) <> " coins")
        & unlines
    in
      pairs

coinCount :: Change -> Int
coinCount = unCount . sum . unChange

coinToChange :: Coin -> Change
coinToChange coin = Change $ M.insert coin (Count 1) M.empty

coinToConvolution :: Coin -> Convolution
coinToConvolution coin@(Coin c) =
  let
    noCoin =  M.insert (Money 0) mempty
    withCoin = M.insert (Money c) (coinToChange coin)
  in
    Convolution $ (noCoin . withCoin) mempty

mergeCoins :: Currency -> Convolution
mergeCoins = merge . map coinToConvolution . unCurrency

merge :: forall a. Monoid a => [a] -> a
merge xs = go xs $ length xs
  where
    go :: [a] -> Int -> a
    go [] _ = mempty
    go [x] _ = x
    go xs n =
      let
        (q, r) = n `divMod` 2
        (left, right@(h : rest)) = splitAt q xs
      in
        if r == 0
        then
          go (zipWith (<>) left right) q
        else
          go (h : zipWith (<>) left rest) (q + 1)

--
dollar = Coin 100
quarter = Coin 25
dime = Coin 10
nickel = Coin 5
penny = Coin 1

coinsUSA = Currency [ quarter, dime, nickel, penny]

step :: Coin -> Int -> ([Coin], Int)
step coin change =
  let (q, r) = change `divMod` (unCoin coin)
  in (replicate q coin, r)

changeS :: Coin -> State Int [Coin]
changeS = state . step

makeChange :: Int -> [Coin]
makeChange = makeChangeWith coinsUSA

makeChangeWith :: Currency -> Int -> [Coin]
makeChangeWith (Currency coins) change =
  let
    currency = sortOn (Down . unCoin) coins
    f = runState $ traverse changeS currency
  in
    concat . fst . f $ change
