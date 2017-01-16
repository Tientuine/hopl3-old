{-
 -  PROC/Interp.hs
 -
 -  Reference implementation of the toy language PROC by Mitchell Wand.
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module PROC.Interp (interp,runInterp,prelude) where

import           Prelude hiding (exp)
import           System.Environment

import           Common.Types

import           PROC.AST
import           PROC.Closure
import           PROC.Environment
import           PROC.Parser
import           PROC.Prelude
import           PROC.Val

runInterp :: IO ()
runInterp = do
    args <- getArgs
    if null args
        then putStrLn "PROC.Interp: Missing source file name"
        else do prog <- readFile $ head args
                print $ interp prog prelude
                return ()

{- Shim for pre-interpreting the prelude definitions -}
compileOne :: Def -> Env -> Env
compileOne (name,src) ρ = extendEnv ρ name (interp src ρ)

prelude :: Env
prelude = foldr compileOne emptyEnv preludeSrc

{- Top-level interpreter routine -}
interp :: Source -> Env -> ExpVal
interp s ρ = valueOfProgram (parse s) ρ

-- semantic reduction of a program
valueOfProgram :: Pgm -> Env -> ExpVal
valueOfProgram (Pgm exp) ρ = valueOf exp ρ

-- semantic reductions for expressions
valueOf :: Exp -> Env -> ExpVal

valueOf (ConstExp n) _ = NumVal n

valueOf (VarExp x) ρ = applyEnv ρ x

valueOf (IsZeroExp exp₁) ρ = BoolVal (n == 0)
  where
    NumVal n = valueOf exp₁ ρ

valueOf (DiffExp exp₁ exp₂) ρ = NumVal (n₁ - n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ

valueOf (LetExp x rhs body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv ρ x v
    v  = valueOf rhs ρ

valueOf (IfExp exp₁ exp₂ exp₃) ρ
    | testIsTrue = valueOf exp₂ ρ
    | otherwise  = valueOf exp₃ ρ
  where
    BoolVal testIsTrue = valueOf exp₁ ρ

valueOf (ProcExp param body) ρ = ProcVal (Proc param body ρ)

valueOf (CallExp rator rand) ρ = applyProcedure proc arg
  where
    ProcVal proc = valueOf rator ρ
    arg          = valueOf rand ρ

{--- Auxiliary functions ---}

applyProcedure :: Proc -> ExpVal -> ExpVal
applyProcedure proc arg = valueOf body (extendEnv savedEnv param arg)
  where
    Proc param body savedEnv = proc

