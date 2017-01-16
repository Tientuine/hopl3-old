{-
 -  MUTABLE_PAIRS/Closure.hs
 -
 -  Reference implementation of the toy language MUTABLE_PAIRS by Mitchell Wand.
 -  This module provides a representation for closures of first-class functions.
 -
 -  Author: Matthew A Johnson
 -}
module MUTABLE_PAIRS.Closure where

import           Common.Types    (Id)

import           MUTABLE_PAIRS.AST
import {-# SOURCE #-} MUTABLE_PAIRS.Environment    (Env)

data Proc
    = Proc    { getParam :: Id, getBody :: Exp, getEnv :: Env }
    | RecProc { getParam :: Id, getBody :: Exp }

instance Show Proc where
    show proc = "proc(" ++ getParam proc ++ "){" ++ show (getBody proc) ++ "}"

