{-
 -  LET/Val.hs
 -
 -  Reference implementation of the toy language LET by Mitchell Wand.
 -  This module provides types for representing the expressed values of
 -  the language.
 -
 -  Author: Matthew A Johnson
 -}
module LET.Val where

type DenVal = ExpVal

-- ADT for represented expressed values
data ExpVal
    = NumVal  { expvalToNum  :: Int }
    | BoolVal { expvalToBool :: Bool }

instance Show ExpVal where
    show (NumVal  n) = "(NumVal "  ++ show n ++ ")"
    show (BoolVal z) = "(BoolVal " ++ show z ++ ")"

