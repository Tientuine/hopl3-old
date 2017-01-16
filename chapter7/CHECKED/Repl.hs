{-
 -  CHECKED/Repl.hs
 -
 -  Reference implementation of the toy language CHECKED by Mitchell Wand.
 -  This module provides an interactive Read-Eval-Print-Loop for the language.
 -
 -  Author: Matthew A Johnson
 -}
import           Control.Exception
import           Control.Monad
import           System.IO

import           CHECKED.Environment
import           CHECKED.Interp
import           CHECKED.Type
import           CHECKED.Val

-- interpreter implementation
main :: IO ()
main = repl initEnv

initEnv :: (Env,TEnv)
initEnv = (extendEnv' ρ [("i",NumVal 1),("v",NumVal 5),("x",NumVal 10)],
           extendEnv' τ [("i",IntType ),("v",IntType ),("x",IntType  )])
  where
    (ρ,τ) = prelude

repl :: (Env,TEnv) -> IO ()
repl ρτ = do
    putStr "CHECKED> "
    hFlush stdout
    prog <- getLine
    when (prog /= ":q") $ do
        unless (null prog) $
            print (checkAndInterp prog ρτ)
                `catch` (\e -> do
                          let err = show (e :: ErrorCall)
                          hPutStr stderr ("\n<repl>: " ++ err ++ "\n")
                          return ())
        repl ρτ

