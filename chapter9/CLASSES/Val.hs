{-
 -  IMPLICIT_REFS/Val.hs
 -
 -  Reference implementation of the toy language IMPLICIT_REFS by Mitchell
 -  Wand. This module provides types for representing the expressed values
 -  of the language.
 -
 -  Author: Matthew A Johnson
 -}
module CLASSES.Val (ExpVal(..),DenVal(..),StoVal,Id) where

import                Common.Types (Id)

import                CLASSES.AST  (Exp)
import                CLASSES.ClassEnv (Object)
import                CLASSES.Closure (Proc)
import {-# SOURCE #-} CLASSES.Store (Reference)

data DenVal = RefVal Reference | Label Id deriving Show
type StoVal = ExpVal

-- ADT for represented expressed values
data ExpVal = NumVal  { expvalToNum  :: Int }
            | BoolVal { expvalToBool :: Bool }
            | ProcVal { expvalToProc :: Proc }
            | ListVal { expvalToList :: [ExpVal] }
            | ObjVal  { expvalToObj  :: Object }

instance Show ExpVal  where
    show (NumVal  n)  = show n
    show (BoolVal z)  = show z
    show (ProcVal f)  = "[#procedure]"
    show (ListVal vs) = show vs
    show (ObjVal obj) = "[#object]"

