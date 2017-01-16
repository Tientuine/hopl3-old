{-
 -  IMPLICIT_REFS/Val.hs
 -
 -  Reference implementation of the toy language IMPLICIT_REFS by Mitchell
 -  Wand. This module provides types for representing the expressed values
 -  of the language.
 -
 -  Author: Matthew A Johnson
 -}
module IMPLICIT_REFS.Val where

import           IMPLICIT_REFS.Closure (Proc)
import {-# SOURCE #-} IMPLICIT_REFS.Store   (Reference)

type DenVal = Reference
type StoVal = ExpVal

-- ADT for represented expressed values
data ExpVal
    = NumVal  { expvalToNum  :: Int }
    | BoolVal { expvalToBool :: Bool }
    | ProcVal { expvalToProc :: Proc }

instance Show ExpVal where
    show (NumVal  n) = "(NumVal "  ++ show n ++ ")"
    show (BoolVal z) = "(BoolVal " ++ show z ++ ")"
    show (ProcVal f) = "(ProcVal " ++ show f ++ ")"

