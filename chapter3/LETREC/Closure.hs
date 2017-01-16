{-
 -  LETREC/Closure.hs
 -
 -  Reference implementation of the toy language LETREC by Mitchell Wand.
 -  This module provides a representation for closures of first-class functions.
 -
 -  Author: Matthew A Johnson
 -}
module LETREC.Closure where

import           Common.Types    (Id)

import           LETREC.AST
import {-# SOURCE #-} LETREC.Environment    (Env)

data Proc
    = Proc    { getParam :: Id, getBody :: Exp, getEnv :: Env }
    | RecProc { getParam :: Id, getBody :: Exp }

instance Show Proc where
    show proc = "proc(" ++ getParam proc ++ "){" ++ show (getBody proc) ++ "}"

