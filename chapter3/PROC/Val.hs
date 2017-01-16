{-
 -  PROC/Val.hs
 -
 -  Reference implementation of the toy language PROC by Mitchell Wand.
 -  This module provides types for representing the expressed values of
 -  the language.
 -
 -  Author: Matthew A Johnson
 -}
module PROC.Val where

import           PROC.Closure    (Proc)

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

