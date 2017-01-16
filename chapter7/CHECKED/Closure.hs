{-
 -  CHECKED/Closure.hs
 -
 -  Reference implementation of the toy language CHECKED by Mitchell Wand.
 -  This module provides a representation for closures of first-class functions.
 -
 -  Author: Matthew A Johnson
 -}
module CHECKED.Closure where

import           Common.Types    (Id)

import           CHECKED.AST
import {-# SOURCE #-} CHECKED.Environment    (Env)

data Proc
    = Proc    { getParam :: Id, getBody :: Exp, getEnv :: Env }
    | RecProc { getParam :: Id, getBody :: Exp }

instance Show Proc where
    show proc = "proc(" ++ getParam proc ++ "){" ++ show (getBody proc) ++ "}"

