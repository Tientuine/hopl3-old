{-
 -  EXPLICIT_REFS/Repl.hs
 -
 -  Reference implementation of the toy language EXPLICIT_REFS by Mitchell Wand.
 -  This module provides an interactive Read-Eval-Print-Loop for the language.
 -
 -  Author: Matthew A Johnson
 -}
import           Control.Exception
import           Control.Monad
import           System.IO

import           EXPLICIT_REFS.Environment
import           EXPLICIT_REFS.Interp
import           EXPLICIT_REFS.Store
import           EXPLICIT_REFS.Val

-- interpreter implementation
main :: IO ()
main = repl initEnv initStore

initStore :: Store
initStore = emptyStore

initEnv :: Env
initEnv = extendEnv' prelude [("i",NumVal 1),("v",NumVal 5),("x",NumVal 10)]

repl :: Env -> Store -> IO ()
repl ρ σ = do
    putStr "EXPLICIT_REFS> "
    hFlush stdout
    prog <- getLine
    when (prog /= ":q") $ do
        unless (null prog) $
            print (interp prog ρ σ)
                `catch` (\e -> do
                          let err = show (e :: ErrorCall)
                          hPutStr stderr ("\n<repl>: " ++ err ++ "\n")
                          return ())
        repl ρ σ

