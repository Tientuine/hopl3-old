{-
 -  CHECKED/Interp.hs
 -
 -  Reference implementation of the toy language CHECKED by Mitchell Wand.
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module CHECKED.Interp (interp,checkAndInterp,runInterp,prelude) where

import           Prelude hiding (exp)
import           System.Environment

import           Common.Types

import           CHECKED.AST
import           CHECKED.Checker
import           CHECKED.Closure
import           CHECKED.Environment
import           CHECKED.Parser
import           CHECKED.Prelude
import           CHECKED.Val

runInterp :: IO ()
runInterp = do
    args <- getArgs
    if null args
        then putStrLn "CHECKED.Interp: Missing source file name"
        else do prog <- readFile $ head args
                print $ checkAndInterp prog prelude
                return ()

{- Shim for pre-interpreting the prelude definitions -}
compileOne :: Def -> (Env,TEnv) -> (Env,TEnv)
compileOne (name,src) (ρ,τ) = (ρ',τ')
  where
    ρ'   = extendEnv ρ name (valueOfProgram prog ρ)
    τ'   = extendEnv τ name ( typeOfProgram prog τ)
    prog = parse src

prelude :: (Env,TEnv)
prelude = foldr compileOne (emptyEnv,emptyEnv) preludeSrc

checkAndInterp :: Source -> (Env,TEnv) -> ExpVal
checkAndInterp s (ρ,τ) = valueOfProgram (check s τ) ρ

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

valueOf (IsZeroExp exp) ρ = BoolVal (n == 0)
  where
    NumVal n = valueOf exp ρ

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

valueOf (ProcExp param _ body) ρ = ProcVal (Proc param body ρ)

valueOf (CallExp rator rand) ρ = applyProcedure proc arg
  where
    ProcVal proc = valueOf rator ρ
    arg          = valueOf rand ρ

valueOf (LetrecExp _ pname bvar _ pbody body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv ρ pname (ProcVal $ RecProc bvar pbody)

{--- Auxiliary functions ---}

applyProcedure :: Proc -> ExpVal -> ExpVal
applyProcedure proc arg = valueOf body (extendEnv savedEnv param arg)
  where
    Proc param body savedEnv = proc

