{-
 -  MUTABLE_PAIRS/Environment.hs
 -
 -  Reference implementation of the toy language MUTABLE_PAIRS by Mitchell
 -  Wand. This module provides an abstract data type for symbol-to-value
 -  mappings. We include several implementations for illustrative purposes,
 -  including a recursive data-structure, an association-list, and an
 -  efficient ribcage representation.
 -
 -  Author: Matthew A Johnson
 -}
module CLASSES.Environment (
    Env,emptyEnv,applyEnv,extendEnv,extendEnv'
  ) where

import Debug.Trace

import           Common.Types    (Id)

import           CLASSES.Val    (DenVal)

nobinding :: Id -> a
nobinding = error . ("No binding found for \""++) . (++"\"")

type Binding = (Id,DenVal)

{- Recursive "data structure" representation for environments -}
data Env = EmptyEnv | Env Id DenVal Env

{- Interface for an environment (symbol-to-value mapping) -}

-- construct an emptyEnv environment
emptyEnv :: Env
emptyEnv = EmptyEnv

-- extract from an environment the mapped value if search symbol is present
applyEnv :: Env -> Id -> DenVal
applyEnv EmptyEnv y = nobinding y
applyEnv (Env x v e) y
    | x == y    = v
    | otherwise = applyEnv e y

-- construct new environment from existing environment plus a new binding
extendEnv :: Id -> DenVal -> Env -> Env
extendEnv x v e = Env x v e

-- construct new environment from existing environment plus new bindings
extendEnv' :: [Id] -> [DenVal] -> Env -> Env
extendEnv' (x:xs) (v:vs) e = extendEnv' xs vs (extendEnv x v e)
extendEnv' _ _ e = e

envToList :: Env -> [Binding]
envToList EmptyEnv = []
envToList (Env x v savedEnv) = (x,v) : envToList savedEnv

instance Show Env where
    show env = show $ envToList env

