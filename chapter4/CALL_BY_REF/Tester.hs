{-
 -  CALL_BY_REF/Tester.hs
 -
 -  Reference implementation of the toy language CALL_BY_REF by Mitchell Wand.
 -  This module provides a non-interactive program that runs a variety
 -  of pre-defined test cases to validate the interpreter.
 -
 -  Author: Matthew A Johnson
 -}
import           Control.Exception

import           Common.Types

import           CALL_BY_REF.Environment
import           CALL_BY_REF.Interp
import           CALL_BY_REF.Store
import           CALL_BY_REF.Val

import           Test.Base
import           Test.CALL_BY_REF

testStore :: Store
testStore = snd prelude ++ [NumVal 1,NumVal 5,NumVal 10]

testEnv :: Env
testEnv = extendEnv' (fst prelude) [("i",n),("v",n+1),("x",n+2)]
  where
    n = length $ snd prelude

main :: IO ()
main = testAll testList where
    testAll [] = return ()
    testAll (t:ts) = do
        case t of
            NumTest  name prog soln -> putStrLn $ test expvalToNum  soln prog name
            BoolTest name prog soln -> putStrLn $ test expvalToBool soln prog name
            ErrTest  name prog      -> putStrLn ("FAILED! (" ++ name ++ " = " ++ show (interp prog (testEnv,testStore)))
                `catch` (\e -> do let _ = show (e :: SomeException)
                                  putStrLn $ "success (" ++ name ++ ")")
        testAll ts

test :: Eq a => (ExpVal -> a) -> a -> Source -> Id -> String
test cast soln prog name = result ++ " (" ++ name ++ ")" where
    expval = interp prog (testEnv,testStore)
    result = if cast expval == soln then "success" else "FAILED!"

