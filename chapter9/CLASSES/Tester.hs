{-
 -  PROC/Tester.hs
 -
 -  Reference implementation of the toy language PROC by Mitchell Wand.
 -  This module provides a non-interactive program that runs a variety
 -  of pre-defined test cases to validate the interpreter.
 -
 -  Author: Matthew A Johnson
 -}
import           Control.Exception

import           CLASSES.Environment
import           CLASSES.Interp
import           CLASSES.Store
import           CLASSES.Val

import           Test.Base
import           Test.CLASSES

testStore :: Store
testStore = snd $ newrefs [NumVal 1,NumVal 5,NumVal 10] (freestore 1000 undefined)

testEnv :: Env
testEnv = extendEnv' ["i","v","x"] [RefVal 0,RefVal 1,RefVal 2] emptyEnv

main :: IO ()
main = testAll testList
  where
    testAll [] = return ()
    testAll (t:ts) = do
        case t of
            NumTest  name prog soln -> putStrLn $ test expvalToNum  soln prog name
            BoolTest name prog soln -> putStrLn $ test expvalToBool soln prog name
            ListNumTest  name prog soln -> putStrLn $ test' expvalToNum  soln prog name
            ListBoolTest name prog soln -> putStrLn $ test' expvalToBool soln prog name
            ErrTest  name prog -> do
                putStrLn $ "FAILED! (" ++ name ++ " = " ++ (show $ interp prog (testEnv,testStore))
                `catch` (\ e -> do let err = show (e :: SomeException)
                                   putStrLn $ "success (" ++ name ++ ")")
        testAll ts

test :: Eq a => (ExpVal -> a) -> a -> String -> String -> String
test cast soln prog name = result ++ " (" ++ name ++ ")" where
    expval = interp prog (testEnv,testStore)
    result = if cast expval == soln then "success" else "FAILED!"

test' :: Eq a => (ExpVal -> a) -> [a] -> String -> String -> String
test' cast soln prog name = result ++ " (" ++ name ++ ")" where
    ListVal expvals = interp prog (testEnv,testStore)
    result = if map cast expvals == soln then "success" else "FAILED!"

