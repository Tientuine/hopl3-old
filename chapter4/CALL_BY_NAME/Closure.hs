{-
 -  CALL_BY_NAME/Closure.hs
 -
 -  Reference implementation of the toy language CALL_BY_NAME by Mitchell Wand.
 -  This module provides a representation for closures of first-class functions.
 -
 -  Author: Matthew A Johnson
 -}
module CALL_BY_NAME.Closure where

import           Common.Types    (Id)

import           CALL_BY_NAME.AST
import {-# SOURCE #-} CALL_BY_NAME.Environment    (Env)

data Proc
    = Proc    { getParam :: Id, getBody :: Exp, getEnv :: Env }
    | RecProc { getParam :: Id, getBody :: Exp }

instance Show Proc where
    show proc = "proc(" ++ getParam proc ++ "){" ++ show (getBody proc) ++ "}"

