{-
 -  IMPLICIT_REFS/Closure.hs
 -
 -  Reference implementation of the toy language IMPLICIT_REFS by Mitchell Wand.
 -  This module provides a representation for closures of first-class functions.
 -
 -  Author: Matthew A Johnson
 -}
module IMPLICIT_REFS.Closure where

import           Common.Types    (Id)

import           IMPLICIT_REFS.AST
import {-# SOURCE #-} IMPLICIT_REFS.Environment    (Env)

data Proc
    = Proc    { getParam :: Id, getBody :: Exp, getEnv :: Env }
    | RecProc { getParam :: Id, getBody :: Exp }

instance Show Proc where
    show proc = "proc(" ++ getParam proc ++ "){" ++ show (getBody proc) ++ "}"

