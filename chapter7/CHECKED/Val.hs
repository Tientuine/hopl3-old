{-
 -  CHECKED/Val.hs
 -
 -  Reference implementation of the toy language CHECKED by Mitchell Wand.
 -  This module provides types for representing the expressed values of
 -  the language.
 -
 -  Author: Matthew A Johnson
 -}
module CHECKED.Val where

import {-# SOURCE #-} CHECKED.Closure    (Proc)

type DenVal = ExpVal

-- ADT for represented expressed values
data ExpVal
    = NumVal  { expvalToNum  :: Int }
    | BoolVal { expvalToBool :: Bool }
    | ProcVal { expvalToProc :: Proc }

instance Show ExpVal where
    show (NumVal  n) = "(NumVal "  ++ show n ++ ")"
    show (BoolVal z) = "(BoolVal " ++ show z ++ ")"
    show (ProcVal f) = "(ProcVal " ++ show f ++ ")"

