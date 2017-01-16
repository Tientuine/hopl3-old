{-
 -  PROC/Repl.hs
 -
 -  Reference implementation of the toy language PROC by Mitchell Wand.
 -  This module provides an interactive Read-Eval-Print-Loop for the language.
 -
 -  Author: Matthew A Johnson
 -}
import           Control.Exception
import           Control.Monad
import           System.IO

import           PROC.Environment
import           PROC.Interp
import           PROC.Val

-- interpreter implementation
main :: IO ()
main = repl initEnv

initEnv :: Env
initEnv = extendEnv' prelude [("i",NumVal 1),("v",NumVal 5),("x",NumVal 10)]

repl :: Env -> IO ()
repl ρ = do
    putStr "PROC> "
    hFlush stdout
    prog <- getLine
    when (prog /= ":q") $ do
        unless (null prog) $
            print (interp prog ρ)
                `catch` (\e -> do
                          let err = show (e :: ErrorCall)
                          hPutStr stderr ("\n<repl>: " ++ err ++ "\n")
                          return ())
        repl ρ

