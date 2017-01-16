{-
 -  IMPLICIT_REFS/Interp.hs
 -
 -  Reference implementation of the toy language IMPLICIT_REFS by Mitchell
 -  Wand. This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module IMPLICIT_REFS.Interp (interp,runInterp,prelude,Answer) where

import           Prelude hiding (exp)
import           System.Environment

import           Common.Types

import           IMPLICIT_REFS.AST
import           IMPLICIT_REFS.Closure
import           IMPLICIT_REFS.Environment
import           IMPLICIT_REFS.Parser
import           IMPLICIT_REFS.Prelude
import           IMPLICIT_REFS.Store
import           IMPLICIT_REFS.Val

data Answer = Answer { getVal :: ExpVal, getStore :: Store }

runInterp :: IO ()
runInterp = do
    args <- getArgs
    if null args
        then putStrLn "IMPLICIT_REFS: Missing source file name"
        else do prog <- readFile $ head args
                print $ interp prog prelude
                return ()

{- Shim for pre-interpreting the prelude definitions -}

compileOne :: Def -> (Env,Store) -> (Env,Store)
compileOne (name,src) (ρ,σ) = (ρ',σ₁)
  where
    (addr,σ₁) = newref (interp src (ρ,σ)) σ
    ρ'        = extendEnv ρ name addr

prelude :: (Env,Store)
prelude = foldr compileOne (emptyEnv,emptyStore) preludeSrc

{- Top-level interpreter -}

interp :: Source -> (Env,Store) -> ExpVal
interp pgm (ρ,σ) = valueOfProgram (parse pgm) ρ σ

-- semantic reduction of a program
valueOfProgram :: Pgm -> Env -> Store -> ExpVal
valueOfProgram (Pgm exp) ρ σ = getVal $ valueOf exp ρ σ

-- semantic reductions for expressions
valueOf :: Exp -> Env -> Store -> Answer

valueOf (ConstExp n) ρ σ = Answer (NumVal n) σ

valueOf (VarExp x) ρ σ = Answer (deref addr σ) σ
  where
    addr = applyEnv ρ x

valueOf (IsZeroExp rand) ρ σ = Answer (BoolVal (n == 0)) σ₁
  where
    Answer (NumVal n) σ₁ = valueOf rand ρ σ

valueOf (DiffExp rand₁ rand₂) ρ σ = Answer (NumVal (n₁ - n₂)) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf rand₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf rand₂ ρ σ₁

valueOf (LetExp x rhs body) ρ σ = valueOf body ρ' σ₂
  where
    ρ'          = extendEnv ρ x addr
    (addr,σ₂)   = newref v σ₁
    Answer v σ₁ = valueOf rhs ρ σ

valueOf (IfExp test conseq altern) ρ σ
    | testIsTrue = valueOf conseq ρ σ₁
    | otherwise  = valueOf altern ρ σ₁
  where
    Answer (BoolVal testIsTrue) σ₁ = valueOf test ρ σ

valueOf (ProcExp param body) ρ σ = Answer (ProcVal (Proc param body ρ)) σ

valueOf (CallExp rator rand) ρ σ = applyProcedure proc arg σ₂
  where
    Answer (ProcVal proc) σ₁ = valueOf rator ρ σ
    Answer arg σ₂            = valueOf rand  ρ σ₁

valueOf (LetrecExp pname bvar pbody body) ρ σ = valueOf body ρ' σ₂
  where
    (addr,σ₁) = newref undefined σ
    ρ'        = extendEnv ρ pname addr
    σ₂        = setref addr (ProcVal (Proc bvar pbody ρ')) σ₁

valueOf (AssignExp var rhs) ρ σ = Answer (NumVal 42) σ₂
  where
    Answer rval σ₁ = valueOf rhs ρ σ
    σ₂             = setref (applyEnv ρ var) rval σ₁

valueOf (BeginExp []) _ _ = undefined
valueOf (BeginExp (exp:exps)) ρ σ
    | null exps = Answer v σ₁
    | otherwise = valueOf (BeginExp exps) ρ σ₁
  where
    Answer v σ₁ = valueOf exp ρ σ

{--- Auxiliary functions ---}

applyProcedure :: Proc -> ExpVal -> Store -> Answer
applyProcedure proc arg σ = valueOf body ρ' σ₁
  where
    Proc param body ρ = proc
    (addr,σ₁)         = newref arg σ
    ρ'                = extendEnv ρ param addr

