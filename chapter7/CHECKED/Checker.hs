{-
 -  CHECKED/Checker.hs
 -
 -  Reference implementation of the toy language CHECKED by Mitchell Wand.
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module CHECKED.Checker (check,typeOfProgram) where

import           Prelude hiding (exp)

import           Common.Types

import           CHECKED.AST
import           CHECKED.Environment
import           CHECKED.Parser
import           CHECKED.Type

check :: Source -> TEnv -> Pgm
check s ρ = case typeOfProgram prog ρ of
    NoType msg -> error msg
    _          -> prog
  where
    prog = parse s

reportUnequalTypes :: Type -> Type -> Exp -> Type
reportUnequalTypes t₁ t₂ exp₁ = NoType $
    "Types didn't match: " ++ show t₁ ++ " /= " ++ show t₂
        ++ " in " ++ (show . show) exp₁

typeOfProgram :: Pgm -> TEnv -> Type
typeOfProgram (Pgm exp) ρ = typeOf exp ρ

typeOf :: Exp -> TEnv -> Type

typeOf (ConstExp _) _ = IntType

typeOf (VarExp x) ρ = applyEnv ρ x

typeOf (IsZeroExp exp) ρ
    | t == IntType = BoolType
    | otherwise    = reportUnequalTypes IntType t exp
  where
    t = typeOf exp ρ

typeOf (DiffExp exp₁ exp₂) ρ
    | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
    | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
    | otherwise     = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ

typeOf (LetExp x rhs body) ρ = typeOf body ρ'
  where
    ρ' = extendEnv ρ x t
    t  = typeOf rhs ρ

typeOf (IfExp exp₁ exp₂ exp₃) ρ
    | t₁ /= BoolType = reportUnequalTypes BoolType t₁ exp₁
    | t₂ /= t₃       = reportUnequalTypes t₂ t₃ exp₂
    | otherwise      = t₂
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ
    t₃ = typeOf exp₃ ρ

typeOf (ProcExp param targ body) ρ = ProcType targ tres
  where
    tres = typeOf body ρ'
    ρ'   = extendEnv ρ param targ

typeOf (CallExp rator rand) ρ
    | targ == targ' = tres
    | otherwise     = reportUnequalTypes targ targ' rand
  where
    ProcType targ tres = typeOf rator ρ
    targ'              = typeOf rand  ρ

typeOf (LetrecExp tres pname param targ pbody body) ρ
    | tres' == tres = typeOf body ρ'
    | otherwise     = reportUnequalTypes tres tres' pbody
  where
    ρ'    = extendEnv ρ pname (ProcType targ tres)
    tres' = typeOf pbody (extendEnv ρ' param targ)

