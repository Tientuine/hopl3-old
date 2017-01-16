{-
 -  CALL_BY_REF/Val.hs
 -
 -  Reference implementation of the toy language CALL_BY_REF by Mitchell
 -  Wand. This module provides types for representing the expressed values
 -  of the language.
 -
 -  Author: Matthew A Johnson
 -}
module CALL_BY_REF.Val where

import           CALL_BY_REF.Closure (Proc)
import           CALL_BY_REF.Pair    (MutPair)
import {-# SOURCE #-} CALL_BY_REF.Store   (Reference)

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

