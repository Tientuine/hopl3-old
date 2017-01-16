{-# LANGUAGE FlexibleInstances #-}

module Sec_2_1 where

class Natural n where
  -- zero = ⌈0⌉
  zero :: n
  -- is_zero ⌈n⌉ = { True   n = 0
  --               { False  n ≠ 0
  is_zero :: n -> Bool
  -- successor ⌈n⌉ = ⌈n+1⌉  (n ≥ 0)
  successor :: n -> n
  -- predecessor ⌈n+1⌉ = ⌈n⌉ (n ≥ 0)
  predecessor :: n -> n

plus :: Natural n => n -> n -> n
plus x y = if is_zero x then y else successor (plus (predecessor x) y)

-- Unary representation, p.33
instance Natural [Bool] where
  zero = []
  is_zero n = null n  -- or just is_zero = null
  successor n = True : n
  predecessor n = tail n  -- or simply predecessor = tail

-- Haskell integer representation, p.33
instance Natural Int where
  zero = 0
  is_zero n = n == 0  -- or simply is_zero = (== 0)
  successor n = n + 1  -- or simply successor = (+ 1)
  predecessor n = n - 1  -- or simply predecessor = (+ (-1))
   
-- Reverse-number representation
-- Represent n by the Haskell number 5-n
instance Natural Integer where
  zero = 5
  is_zero n = n == 5
  successor n = n - 1
  predecessor n = n + 1


