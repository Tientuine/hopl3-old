{-
 -  EXPLICIT_REFS/Val.hs
 -
 -  Reference implementation of the toy language EXPLICIT_REFS by Mitchell
 -  Wand. This module provides types for representing the expressed values
 -  of the language.
 -
 -  Author: Matthew A Johnson
 -}
module EXPLICIT_REFS.Val where

import           EXPLICIT_REFS.Closure (Proc)
import {-# SOURCE #-} EXPLICIT_REFS.Store   (Reference)

type DenVal = ExpVal
type StoVal = ExpVal

-- ADT for represented expressed values
data ExpVal
    = NumVal  { expvalToNum  :: Int }
    | BoolVal { expvalToBool :: Bool }
    | ProcVal { expvalToProc :: Proc }
    | RefVal  { expvalToRef  :: Reference }

instance Show ExpVal where
    show (NumVal  n) = "(NumVal "  ++ show n ++ ")"
    show (BoolVal z) = "(BoolVal " ++ show z ++ ")"
    show (ProcVal f) = "(ProcVal " ++ show f ++ ")"
    show (RefVal  r) = "(RefVal "  ++ show r ++ ")"

