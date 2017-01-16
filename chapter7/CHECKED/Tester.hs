{-
 -  CHECKED/Tester.hs
 -
 -  Reference implementation of the toy language CHECKED by Mitchell Wand.
 -  This module provides a non-interactive program that runs a variety
 -  of pre-defined test cases to validate the interpreter.
 -
 -  Author: Matthew A Johnson
 -}
import           Control.Exception

import           Common.Types

import           CHECKED.Environment
import           CHECKED.Checker
import           CHECKED.Interp
import           CHECKED.Parser
import           CHECKED.Type
import           CHECKED.Val

import           Test.Base
import           Test.CHECKED

testEnv :: TEnv --(Env,TEnv)
testEnv = snd (extendEnv' ρ [("i",NumVal 1),("v",NumVal 5),("x",NumVal 10)],
               extendEnv' τ [("i",IntType ),("v",IntType ),("x",IntType  )])
  where
    (ρ,τ) = prelude

main :: IO ()
main = checkAll checkList
  where
    checkAll [] = return ()
    checkAll (t:ts) = do
        case t of
            PassCheck  name prog soln -> putStrLn $ checkOne soln prog name
            FailCheck  name prog      -> case typeOfProgram (parse prog) testEnv of
                                           NoType _ -> putStrLn $ "success (" ++ name ++ ")"
                                           ty       -> putStrLn $ "FAILED! (" ++ show ty ++ ")"
        checkAll ts

checkOne :: String -> Source -> Id -> String
checkOne soln prog name = res ++ " (" ++ name ++ ")"
  where
    ty  = typeOfProgram (parse prog) testEnv
    res = if show ty == soln then "success" else ("FAILED! (" ++ show ty ++ " /= " ++ soln)

