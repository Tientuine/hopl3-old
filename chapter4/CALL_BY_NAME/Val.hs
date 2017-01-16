{-
 -  CALL_BY_NAME/Val.hs
 -
 -  Reference implementation of the toy language CALL_BY_NAME by Mitchell
 -  Wand. This module provides types for representing the expressed values
 -  of the language.
 -
 -  Author: Matthew A Johnson
 -}
module CALL_BY_NAME.Val where

import           CALL_BY_NAME.AST         (Exp)
import           CALL_BY_NAME.Closure     (Proc)
import {-# SOURCE #-} CALL_BY_NAME.Environment (Env)
import           CALL_BY_NAME.Pair        (MutPair)
import {-# SOURCE #-} CALL_BY_NAME.Store       (Reference)

type DenVal = Reference

data StoVal = StoVal ExpVal | Thunk Exp Env

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

