module Tier2.Writer (collectAndSumInOrder) where

import Control.Monad.Writer
import Data.Monoid (Sum(..))
import Tier0.Writer (Tree (..))

collectAndSumInOrder :: Num a => Tree a -> Writer (Sum a) [a]
collectAndSumInOrder (Leaf x) = do
  tell (Sum x)
  return [x]
collectAndSumInOrder (Branch left x right) = do
  leftVals <- collectAndSumInOrder left
  tell (Sum x)
  rightVals <- collectAndSumInOrder right
  return (leftVals ++ [x] ++ rightVals)