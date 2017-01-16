{-
 -  CALL_BY_NEED/Tester.hs
 -
 -  Reference implementation of the toy language CALL_BY_NEED by Mitchell Wand.
 -  This module provides a non-interactive program that runs a variety
 -  of pre-defined test cases to validate the interpreter.
 -
 -  Author: Matthew A Johnson
 -}
import           Control.Exception

import           Common.Types

import           CALL_BY_NEED.Environment
import           CALL_BY_NEED.Interp
import           CALL_BY_NEED.Store
import           CALL_BY_NEED.Val

import           Test.Base
import           Test.CALL_BY_NEED

testStore :: Store
testStore = snd prelude ++ [StoVal (NumVal 1),StoVal (NumVal 5),StoVal (NumVal 10)]

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

