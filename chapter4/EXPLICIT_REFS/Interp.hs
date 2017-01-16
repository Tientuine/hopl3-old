{-
 -  EXPLICIT_REFS/Interp.hs
 -
 -  Reference implementation of the toy language EXPLICIT_REFS by Mitchell
 -  Wand. This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module EXPLICIT_REFS.Interp (interp,runInterp,prelude,Answer) where

import           Prelude hiding (exp)
import           System.Environment

import           Common.Types

import           EXPLICIT_REFS.AST
import           EXPLICIT_REFS.Closure
import           EXPLICIT_REFS.Environment
import           EXPLICIT_REFS.Parser
import           EXPLICIT_REFS.Prelude
import           EXPLICIT_REFS.Store
import           EXPLICIT_REFS.Val

data Answer = Answer { getVal :: ExpVal, getStore :: Store }

runInterp :: IO ()
runInterp = do
    args <- getArgs
    if null args
        then putStrLn "EXPLICIT_REFS.Interp: Missing source file name"
        else do prog <- readFile $ head args
                print $ interp prog prelude emptyStore
                return ()

{- Shim for pre-interpreting the prelude definitions -}
compileOne :: Def -> Env -> Env
compileOne (name,src) ρ = extendEnv ρ name (interp src ρ emptyStore)

prelude :: Env
prelude = foldr compileOne emptyEnv preludeSrc

{- Top-level interpreter routine -}
interp :: Source -> Env -> Store -> ExpVal
interp pgm ρ σ = valueOfProgram (parse pgm) ρ σ

-- semantic reduction of a program
valueOfProgram :: Pgm -> Env -> Store -> ExpVal
valueOfProgram (Pgm exp) ρ σ = getVal (valueOf exp ρ σ)

-- semantic reductions for expressions
valueOf :: Exp -> Env -> Store -> Answer

valueOf (ConstExp n) _ σ = Answer (NumVal n) σ

valueOf (VarExp x) ρ σ = Answer (applyEnv ρ x) σ

valueOf (IsZeroExp rand) ρ σ = Answer (BoolVal (n == 0)) σ₁
  where
    Answer (NumVal n) σ₁ = valueOf rand ρ σ

valueOf (DiffExp rand₁ rand₂) ρ σ = Answer (NumVal (n₁ - n₂)) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf rand₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf rand₂ ρ σ₁

valueOf (LetExp x rhs body) ρ σ = valueOf body ρ' σ₁
  where
    ρ'          = extendEnv ρ x v
    Answer v σ₁ = valueOf rhs ρ σ

valueOf (IfExp test conseq altern) ρ σ
    | testIsTrue  = valueOf conseq ρ σ₁
    | otherwise = valueOf altern ρ σ₁
  where
    Answer (BoolVal testIsTrue) σ₁ = valueOf test ρ σ

valueOf (ProcExp param body) ρ σ = Answer (ProcVal $ Proc param body ρ) σ

valueOf (CallExp rator rand) ρ σ = applyProcedure proc arg σ₂
  where
    Answer (ProcVal proc) σ₁ = valueOf rator ρ σ
    Answer arg σ₂ = valueOf rand ρ σ₁

valueOf (LetrecExp pname bvar pbody body) ρ σ = valueOf body ρ' σ
  where
    ρ' = extendEnv ρ pname (ProcVal (RecProc bvar pbody))

valueOf (NewrefExp rhs) ρ σ = Answer (RefVal addr) σ₂
  where
    Answer val σ₁ = valueOf rhs ρ σ
    (addr,σ₂)     = newref val σ₁

valueOf (DerefExp rhs) ρ σ = Answer val σ₁
  where
    Answer ref σ₁ = valueOf rhs ρ σ
    addr          = expvalToRef ref
    val           = deref addr σ₁

valueOf (SetrefExp lhs rhs) ρ σ = Answer (NumVal 42) σ₃
  where
    Answer lval σ₁ = valueOf lhs ρ σ
    Answer rval σ₂ = valueOf rhs ρ σ₁
    σ₃             = setref (expvalToRef lval) rval σ₂

valueOf (BeginExp []) _ _ = undefined
valueOf (BeginExp (exp:exps)) ρ σ
    | null exps = Answer val σ₁
    | otherwise = valueOf (BeginExp exps) ρ σ₁
  where
    Answer val σ₁ = valueOf exp ρ σ

{--- Auxiliary functions ---}

applyProcedure :: Proc -> ExpVal -> Store -> Answer
applyProcedure proc arg σ = valueOf body ρ' σ
  where
    Proc param body ρ = proc
    ρ'                = extendEnv ρ param arg

