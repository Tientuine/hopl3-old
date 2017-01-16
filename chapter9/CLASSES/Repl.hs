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

import           CLASSES.Environment
import           CLASSES.Interp
import           CLASSES.Prelude
import           CLASSES.Store
import           CLASSES.Val

-- interpreter implementation
main :: IO ()
main = repl prelude

repl :: (Env,Store) -> IO ()
repl (ρ,σ) = do
    putStr "CLASSES> "
    hFlush stdout
    prog <- getLine
    when (prog /= ":q") $ do
        when (not . null $ prog) $ do
            catch (putStrLn $ show $ interp prog (ρ,σ))
                  (\ e -> do let err = show (e :: ErrorCall)
                             hPutStr stderr ("\n<repl>: " ++ err++ "\n")
                             return ())
        repl (ρ,σ)

