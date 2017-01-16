{-
 -  IMPLICIT_REFS/Repl.hs
 -
 -  Reference implementation of the toy language IMPLICIT_REFS by Mitchell Wand.
 -  This module provides an interactive Read-Eval-Print-Loop for the language.
 -
 -  Author: Matthew A Johnson
 -}
import           Control.Exception
import           Control.Monad
import           System.IO

import           IMPLICIT_REFS.Environment
import           IMPLICIT_REFS.Interp
import           IMPLICIT_REFS.Store
import           IMPLICIT_REFS.Val

-- interpreter implementation
main :: IO ()
main = repl (initEnv,initStore)

initStore :: Store
initStore = snd prelude ++ [NumVal 1,NumVal 5,NumVal 10]

initEnv :: Env
initEnv = extendEnv' (fst prelude) [("i",n),("v",n+1),("x",n+2)]
  where
    n = length $ snd prelude

repl :: (Env,Store) -> IO ()
repl (ρ,σ) = do
    putStr "IMPLICIT_REFS> "
    hFlush stdout
    prog <- getLine
    when (prog /= ":q") $ do
        unless (null prog) $
            print (interp prog (ρ,σ))
                `catch` (\e -> do
                          let err = show (e :: ErrorCall)
                          hPutStr stderr ("\n<repl>: " ++ err ++ "\n")
                          return ())
        repl (ρ,σ)

