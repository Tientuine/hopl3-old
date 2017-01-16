{-
 -  LETREC/Environment.hs
 -
 -  Reference implementation of the toy language LETREC by Mitchell Wand.
 -  This module provides an abstract data type for symbol-to-value mappings.
 -  We include several implementations for illustrative purposes, including
 -  a recursive data-structure, an association-list, and an efficient ribcage
 -  representation.
 -
 -  Author: Matthew A Johnson
 -}
module LETREC.Environment (
    Env,emptyEnv,applyEnv,extendEnv,extendEnv'
  ) where

import           Common.Types    (Id)

import           LETREC.Closure
import           LETREC.Val        (DenVal,ExpVal(ProcVal))

nobinding :: Id -> a
nobinding = error . ("No binding found for \""++) . (++"\"")

type Binding = (Id,DenVal)

-- helper function for recursive environments
expand :: Env -> DenVal -> DenVal
expand ρ (ProcVal (RecProc param body)) = ProcVal (Proc param body ρ)
expand _ v = v

{- Recursive "data structure" representation for environments -}
data Env = EmptyEnv | Env Id DenVal Env

{- Interface for an environment (symbol-to-value mapping) -}

-- construct an emptyEnv environment
emptyEnv :: Env
emptyEnv = EmptyEnv

-- extract from an environment the mapped value if search symbol is present
applyEnv :: Env -> Id -> DenVal
applyEnv EmptyEnv y = nobinding y
applyEnv e'@(Env x v e) y
    | x == y    = expand e' v
    | otherwise = applyEnv e y

-- construct new environment from existing environment plus a new binding
extendEnv :: Env -> Id -> DenVal -> Env
extendEnv e x v = Env x v e

-- construct new environment from existing environment plus new bindings
extendEnv' :: Env -> [Binding] -> Env
extendEnv' e ((x,v):xvs) = extendEnv' (extendEnv e x v) xvs
extendEnv' e [] = e

envToList :: Env -> [Binding]
envToList EmptyEnv = []
envToList (Env x v savedEnv) = (x,v) : envToList savedEnv

instance Show Env where
    show env = show $ envToList env

