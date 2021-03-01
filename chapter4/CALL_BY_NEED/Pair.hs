{-
 -  CALL_BY_NEED/Pair.hs
 -
 -  Reference implementation of the toy language CALL_BY_NEED by Mitchell
 -  Wand. This module provides an interface and several representations for
 -  a pair abstraction.
 -
 -  Author: Matthew A Johnson
 -}
module CALL_BY_NEED.Pair (
    MutPair,makePair,left,right,setLeft,setRight
  ) where

import {-# SOURCE #-} CALL_BY_NEED.Val
import {-# SOURCE #-} CALL_BY_NEED.Store

type MutPair = Pair2

makePair :: Store -> StoVal -> StoVal -> (MutPair,Store)
makePair = makePair2

left  :: Store -> MutPair -> StoVal
left  = left2
right :: Store -> MutPair -> StoVal
right = right2

setLeft  :: Store -> MutPair -> StoVal -> Store
setLeft  = setLeft2
setRight :: Store -> MutPair -> StoVal -> Store
setRight = setRight2


{-
 - Representation as a pair of references
 - Advanced Note: The references need not be stored near one another in
 - the free store; however, there are performance implications related
 - to memory locality if we do not keep these references proximal.
 -}
type Pair1 = (Reference,Reference)

makePair1 :: Store -> StoVal -> StoVal -> (Pair1,Store)
makePair1 σ v₁ v₂ = ((addr₁,addr₂),σ₂) where
    (addr₁,σ₁) = newref v₁ σ
    (addr₂,σ₂) = newref v₂ σ₁

left1  :: Store -> Pair1 -> StoVal
left1  σ (addr,_) = deref addr σ
right1 :: Store -> Pair1 -> StoVal
right1 σ (_,addr) = deref addr σ

setLeft1  :: Store -> Pair1 -> StoVal -> Store
setLeft1  σ (addr,_) v = setref addr v σ
setRight1 :: Store -> Pair1 -> StoVal -> Store
setRight1 σ (_,addr) v = setref addr v σ

{-
 - Representation as a reference to the first of two consecutive locations
 - Advanced Note: Ideally, both location should be allocated as part of a
 - single atomic operation. Not doing so would present a problem for
 - multi-threaded languages.
 -}
type Pair2 = Reference

makePair2 :: Store -> StoVal -> StoVal -> (Pair2,Store)
makePair2 σ v₁ v₂ = (addr,σ₂) where
    (addr,σ₁) = newref v₁ σ
    (_,σ₂)    = newref v₂ σ₁

left2  :: Store -> Pair2 -> StoVal
left2  σ addr = deref addr σ
right2 :: Store -> Pair2 -> StoVal
right2 σ addr = deref (addr + 1) σ

setLeft2  :: Store -> Pair2 -> StoVal -> Store
setLeft2  σ addr v = setref addr v σ
setRight2 :: Store -> Pair2 -> StoVal -> Store
setRight2 σ addr v = setref (addr + 1) v σ

