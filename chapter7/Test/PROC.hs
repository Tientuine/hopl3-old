{-
 -  Test/PROC.hs
 -
 -  Test cases for the PROC language.
 -}
module Test.PROC where

import           Test.Base
import qualified Test.LET  as LET

testList :: [Test]
testList = LET.testList ++ [
    NumTest "apply-proc-in-rator-pos" "(proc(x) -(x,1)  30)" 29
  , NumTest "apply-simple-proc" "let f = proc (x) -(x,1) in (f 30)" 29
  , NumTest "let-to-proc-1" "(proc(f)(f 30)  proc(x)-(x,1))" 29
  , NumTest "nested-procs" "((proc (x) proc (y) -(x,y)  5) 6)" (-1)
  , NumTest "nested-procs2" "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)" (-1)
  , NumTest "y-combinator-1" "let fix = proc (f)\
                                  \let d = proc (x) proc (z) ((f (x x)) z)\
                                  \in proc (n) ((f (d d)) n)\
                              \in let t4m = proc (g) proc(x)\
                                                \if zero?(x) then 0\
                                                \else -((g -(x,1)),-4)\
                                  \in let times4 = (fix t4m)\
                                      \in (times4 3)" 12
  , NumTest "scope-of-closure" "let f = let y = 5 in proc (x) -(x, -(0, y)) in (f 7)" 12
  ]

