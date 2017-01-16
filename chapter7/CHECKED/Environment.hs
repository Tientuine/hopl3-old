{-# LANGUAGE TypeSynonymInstances #-}
{-
 -  CHECKED/Environment.hs
 -
 -  Reference implementation of the toy language CHECKED by Mitchell Wand.
 -  This module provides an abstract data type for symbol-to-value mappings.
 -  We include several implementations for illustrative purposes, including
 -  a recursive data-structure, an association-list, and an efficient ribcage
 -  representation.
 -
 -  Author: Matthew A Johnson
 -}
module CHECKED.Environment (
    Env,TEnv,emptyEnv,applyEnv,extendEnv,extendEnv'
  ) where

import           Common.Types    (Id)

import           CHECKED.Closure
import           CHECKED.Val        (DenVal,ExpVal(ProcVal))
import           CHECKED.Type       (Type)

type Env  = Environment DenVal
type TEnv = Environment Type

nobinding :: Id -> a
nobinding = error . ("No binding found for \""++) . (++"\"")

type Binding v = (Id,v)

{- Recursive "data structure" representation for environments -}
data Environment v = EmptyEnv | Env Id v (Environment v)

{- Interface for an environment (symbol-to-value mapping) -}

-- construct an emptyEnv environment
emptyEnv :: Environment v
emptyEnv = EmptyEnv

-- extract from an environment the mapped value if search symbol is present
applyEnv :: Expandable v => Environment v -> Id -> v
applyEnv EmptyEnv y = nobinding y
applyEnv e'@(Env x v e) y
    | x == y    = expand e' v
    | otherwise = applyEnv e y

-- construct new environment from existing environment plus a new binding
extendEnv :: Environment v -> Id -> v -> Environment v
extendEnv e x v = Env x v e

-- construct new environment from existing environment plus new bindings
extendEnv' :: Environment v -> [Binding v] -> Environment v
extendEnv' e ((x,v):xvs) = extendEnv' (extendEnv e x v) xvs
extendEnv' e [] = e

envToList :: Environment v -> [Binding v]
envToList EmptyEnv = []
envToList (Env x v savedEnv) = (x,v) : envToList savedEnv

instance Show v => Show (Environment v) where
    show env = show $ envToList env

class Expandable v where
    -- helper function for recursive environments
    expand :: Environment v -> v -> v

instance Expandable DenVal where
    expand ρ (ProcVal (RecProc param body)) = ProcVal (Proc param body ρ)
    expand _ v = v

instance Expandable Type where
    expand _ v = v

