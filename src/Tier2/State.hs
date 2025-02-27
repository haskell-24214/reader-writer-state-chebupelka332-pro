module Tier2.State where

import Control.Monad.State

data Registers = Registers { ax :: Int, bx :: Int, blink :: Bool, acc :: Int }

emptyRegisters = Registers 0 0 False 0

type Calculation = State Registers Int

plus :: Calculation
plus = undefined

minus :: Calculation
minus = undefined

productS :: Calculation
productS = undefined

div :: Calculation
div = undefined

swap :: Calculation
swap = undefined

blinkS :: Calculation
blinkS = undefined

accS :: Calculation
accS = undefined

number :: Int -> Calculation
number x = undefined

commandToCalculation :: String -> Calculation
commandToCalculation s =
  case s of
    "+" -> plus
    "-" -> minus
    "*" -> productS
    "swap" -> swap
    "blink" -> blinkS
    "acc" -> accS
    x -> number (read x)

buildCalculation :: [String] -> Calculation
buildCalculation xs = 
  foldl (\a x -> a >>= (\_ -> x)) (state (\s -> (0, s))) (map commandToCalculation xs)

calculate :: [String] -> Int
calculate xs = evalState (buildCalculation xs) emptyRegisters
