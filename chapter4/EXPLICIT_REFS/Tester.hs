{-
 -  EXPLICIT_REFS/Tester.hs
 -
 -  Reference implementation of the toy language EXPLICIT_REFS by Mitchell Wand.
 -  This module provides a non-interactive program that runs a variety
 -  of pre-defined test cases to validate the interpreter.
 -
 -  Author: Matthew A Johnson
 -}
import           Control.Exception

import           Common.Types

import           EXPLICIT_REFS.Environment
import           EXPLICIT_REFS.Interp
import           EXPLICIT_REFS.Store
import           EXPLICIT_REFS.Val

import           Test.Base
import           Test.EXPLICIT_REFS

testStore :: Store
testStore = emptyStore

testEnv :: Env
testEnv = extendEnv' prelude [("i",NumVal 1),("v",NumVal 5),("x",NumVal 10)]

main :: IO ()
main = testAll testList where
    testAll [] = return ()
    testAll (t:ts) = do
        case t of
            NumTest  name prog soln -> putStrLn $ test expvalToNum  soln prog name
            BoolTest name prog soln -> putStrLn $ test expvalToBool soln prog name
            ErrTest  name prog      -> putStrLn ("FAILED! (" ++ name ++ " = " ++ show (interp prog testEnv testStore))
                `catch` (\e -> do let _ = show (e :: SomeException)
                                  putStrLn $ "success (" ++ name ++ ")")
        testAll ts

test :: Eq a => (ExpVal -> a) -> a -> Source -> Id -> String
test cast soln prog name = result ++ " (" ++ name ++ ")" where
    expval = interp prog testEnv testStore
    result = if cast expval == soln then "success" else "FAILED!"

