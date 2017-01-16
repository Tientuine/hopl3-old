{-
 -  CALL_BY_NAME/Repl.hs
 -
 -  Reference implementation of the toy language CALL_BY_NAME by Mitchell Wand.
 -  This module provides an interactive Read-Eval-Print-Loop for the language.
 -
 -  Author: Matthew A Johnson
 -}
import           Control.Exception
import           Control.Monad
import           System.IO

import           CALL_BY_NAME.Environment
import           CALL_BY_NAME.Interp
import           CALL_BY_NAME.Store
import           CALL_BY_NAME.Val

-- interpreter implementation
main :: IO ()
main = repl (initEnv,initStore)

initStore :: Store
initStore = snd prelude ++ [StoVal (NumVal 1),StoVal (NumVal 5),StoVal (NumVal 10)]

initEnv :: Env
initEnv = extendEnv' (fst prelude) [("i",n),("v",n+1),("x",n+2)]
  where
    n = length $ snd prelude

repl :: (Env,Store) -> IO ()
repl (ρ,σ) = do
    putStr "CALL_BY_NAME> "
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

