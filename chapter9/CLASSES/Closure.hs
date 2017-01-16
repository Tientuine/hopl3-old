{-
 -  CLASSES/Closure.hs
 -
 -  Reference implementation of the toy language CLASSES by Mitchell Wand.
 -  This module provides a representation for closures of first-class functions.
 -
 -  Author: Matthew A Johnson
 -}
module CLASSES.Closure where

import Common.Types (Id)

import                CLASSES.AST
import {-# SOURCE #-} CLASSES.Environment (Env)

data Proc = Proc    { getparam :: String, getbody :: Exp, getenv :: Env }
          | RecProc { getparam :: String, getbody :: Exp }

instance Show Proc where
    show proc = "proc(" ++ getparam proc ++ "){" ++ show (getbody proc) ++ "}"

