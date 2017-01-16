{-
 -  CALL_BY_REF/Closure.hs
 -
 -  Reference implementation of the toy language CALL_BY_REF by Mitchell Wand.
 -  This module provides a representation for closures of first-class functions.
 -
 -  Author: Matthew A Johnson
 -}
module CALL_BY_REF.Closure where

import           Common.Types    (Id)

import           CALL_BY_REF.AST
import {-# SOURCE #-} CALL_BY_REF.Environment    (Env)

data Proc
    = Proc    { getParam :: Id, getBody :: Exp, getEnv :: Env }
    | RecProc { getParam :: Id, getBody :: Exp }

instance Show Proc where
    show proc = "proc(" ++ getParam proc ++ "){" ++ show (getBody proc) ++ "}"

