{-
 -  LET/Interp.hs
 -
 -  Reference implementation of the toy language LET by Mitchell Wand.
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module LET.Interp (interp,runInterp) where

import           Prelude hiding (exp)
import           System.Environment

import           Common.Types

import           LET.AST
import           LET.Environment
import           LET.Parser
import           LET.Val

runInterp :: IO ()
runInterp = do
    args <- getArgs
    if null args
        then putStrLn "LET.Interp: Missing source file name"
        else do prog <- readFile $ head args
                print $ interp prog emptyEnv
                return ()

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

