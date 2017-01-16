{-
 -  MUTABLE_PAIRS/Val.hs
 -
 -  Reference implementation of the toy language MUTABLE_PAIRS by Mitchell
 -  Wand. This module provides types for representing the expressed values
 -  of the language.
 -
 -  Author: Matthew A Johnson
 -}
module MUTABLE_PAIRS.Val where

import           MUTABLE_PAIRS.Closure (Proc)
import           MUTABLE_PAIRS.Pair    (MutPair)
import {-# SOURCE #-} MUTABLE_PAIRS.Store   (Reference)

type DenVal = Reference
type StoVal = ExpVal

-- ADT for represented expressed values
data ExpVal
    = NumVal  { expvalToNum  :: Int }
    | BoolVal { expvalToBool :: Bool }
    | ProcVal { expvalToProc :: Proc }
    | MutPairVal { expvalToPair :: MutPair }

instance Show ExpVal where
    show (NumVal  n) = "(NumVal "  ++ show n ++ ")"
    show (BoolVal z) = "(BoolVal " ++ show z ++ ")"
    show (ProcVal f) = "(ProcVal " ++ show f ++ ")"
    show (MutPairVal pr) = "(MutPairVal " ++ show pr ++ ")"

