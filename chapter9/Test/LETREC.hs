{-
 -  Test/LETREC.hs
 -
 -  Test cases for the LETREC language.
 -}
module Test.LETREC where

import           Test.Base
import qualified Test.PROC as PROC

testList :: [Test]
testList = PROC.testList ++ [
    NumTest "simple-letrec-1" "letrec f(x) = -(x,1) in (f 33)" 32
  , NumTest "simple-letrec-2"
            "letrec f(x) = if zero?(x) then 0 else -((f -(x,1)), -2) in (f 4)"
            8
  , NumTest "simple-letrec-3"
            "let m = -5\
            \in letrec f(x) = if zero?(x) then 0\
                             \else -((f -(x,1)), m) in (f 4)" 20
  , NumTest "HO-nested-letrecs"
            "letrec even(odd) = proc(x) if zero?(x) then 1 else (odd -(x,1))\
            \in letrec odd(x) = if zero?(x) then 0 else ((even odd) -(x,1))\
               \in (odd 13)" 1
  ]

