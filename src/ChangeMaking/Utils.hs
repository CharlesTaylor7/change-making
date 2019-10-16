module ChangeMaking.Utils where

import Data.Monoid
import Control.Arrow

import Control.Monad.Identity

loop :: (a -> Either a b) -> a -> b
loop act = runIdentity . loopM (Identity . act)

loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM act x = act x >>= loopM act ||| pure

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
