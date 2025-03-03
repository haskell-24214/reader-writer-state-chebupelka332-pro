module Tier2.State where

import Control.Monad.State

data Registers = Registers { ax :: Int, bx :: Int, blink :: Bool, acc :: Int }

emptyRegisters = Registers 0 0 False 0

type Calculation = State Registers Int

plus :: Calculation
plus = state $ \(Registers a b _ _) -> (a + b, Registers a b False (a + b))

minus :: Calculation
minus = state $ \(Registers a b _ _) -> (a - b, Registers a b False (a - b))

productS :: Calculation
productS = state $ \(Registers a b _ _) -> (a * b, Registers a b False (a * b))

divide :: Calculation
divide = state $ \(Registers a b _ _) -> if b == 0
                                        then (0, emptyRegisters)
                                        else (a `Prelude.div` b, Registers a b False (a `Prelude.div` b))

swap :: Calculation
swap = state $ \(Registers a b bl acc) -> (acc, Registers b a bl acc)

blinkS :: Calculation
blinkS = state $ \(Registers a b bl acc) -> (acc, Registers a b (not bl) acc)

accS :: Calculation
accS = state $ \(Registers a b bl acc) -> if not bl
                                         then (acc, Registers acc b (not bl) acc)
                                         else (acc, Registers a acc (not bl) acc)

number :: Int -> Calculation
number x = state $ \(Registers a b bl acc) -> if not bl
                                             then (x, Registers x b (not bl) acc)
                                             else (x, Registers a x (not bl) acc)

commandToCalculation :: String -> Calculation
commandToCalculation s =
  case s of
    "+"    -> plus
    "-"    -> minus
    "*"    -> productS
    "/"    -> divide
    "swap" -> swap
    "blink"-> blinkS
    "acc"  -> accS
    x      -> number (read x)

buildCalculation :: [String] -> Calculation
buildCalculation xs =
  foldl (\a x -> a >>= (\_ -> commandToCalculation x)) (return 0) xs

calculate :: [String] -> Int
calculate xs = evalState (buildCalculation xs) emptyRegisters