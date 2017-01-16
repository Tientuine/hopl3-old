{-
 -  PROC/Closure.hs
 -
 -  Reference implementation of the toy language PROC by Mitchell Wand.
 -  This module provides a representation for closures of first-class functions.
 -
 -  Author: Matthew A Johnson
 -}
module PROC.Closure where

import           Common.Types    (Id)

import           PROC.AST
import {-# SOURCE #-} PROC.Environment    (Env)

data Proc
    = Proc    { getParam :: Id, getBody :: Exp, getEnv :: Env }

instance Show Proc where
    show proc = "proc(" ++ getParam proc ++ "){" ++ show (getBody proc) ++ "}"

