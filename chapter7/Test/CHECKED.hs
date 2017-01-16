{-
 -  Test/CHECKED.hs
 -
 -  Test cases for the CHECKED language.
 -}
module Test.CHECKED where

import           Test.Base

checkList :: [Check]
checkList = [
    PassCheck "positive-const" "11" "int"
  , PassCheck "negative-const" "-33" "int"
  , PassCheck "simple-arith-1" "-(44,33)" "int"
  , PassCheck "nested-arith-left" "-(-(44,33),22)" "int"
  , PassCheck "nested-arith-right" "-(55, -(22,11))" "int"
  , PassCheck "test-var-1" "x" "int"
  , PassCheck "test-var-2" "-(x,1)" "int"
  , PassCheck "test-var-3" "-(1,x)" "int"
  , PassCheck "if-true" "if zero?(0) then 3 else 4" "int"
  , PassCheck "if-false" "if zero?(1) then 3 else 4" "int"
  , FailCheck "no-bool-to-diff-1" "-(zero?(0),1)"
  , FailCheck "no-bool-to-diff-2" "-(1,zero?(0))"
  , FailCheck "no-int-to-if" "if 1 then 2 else 3"
  , PassCheck "if-eval-test-true" "if zero?(-(11,11)) then 3 else 4" "int"
  , PassCheck "if-eval-test-false" "if zero?(-(11, 12)) then 3 else 4" "int"
  , PassCheck "simple-let-1" "let x = 3 in x" "int"
  , PassCheck "eval-let-body" "let x = 3 in -(x,1)" "int"
  , PassCheck "eval-let-rhs" "let x = -(4,1) in -(x,1)" "int"
  , PassCheck "simple-nested-let" "let x = 3 in let y = 4 in -(x,y)" "int"
  , PassCheck "check-shadowing-in-body" "let x = 3 in let x = 4 in x" "int"
  , PassCheck "check-shadowing-in-rhs" "let x = 3 in let x = -(x,1) in x" "int"
  , PassCheck "apply-proc-in-rator-pos" "(proc (x:int) -(x,1)  30)" "int"
  , PassCheck "apply-simple-proc" "let f = proc (x:int) -(x,1) in (f 30)" "int"
  , PassCheck "let-to-proc-1" "(proc (f:(int->int)) (f 30)  proc (x:int) -(x,1))" "int"
  , PassCheck "nested-procs" "((proc (x:int) proc (y:int) -(x,y)  5) 6)" "int"
  , PassCheck "nested-procs2" "let f = proc(x:int) proc (y:int) -(x,y) in ((f -(10,5)) 6)" "int"
  , PassCheck "scope-of-closure" "let f = let y = 5 in proc (x:int) -(x, -(0, y)) in (f 7)" "int"
  , PassCheck "simple-letrec-1" "letrec int f(x:int) = -(x,1) in (f 33)" "int"
  , PassCheck "simple-letrec-2"
            "letrec int f(x:int) = if zero?(x) then 0 else -((f -(x,1)), -2) in (f 4)"
            "int"
  , PassCheck "simple-letrec-3"
            "let m = -5\
            \in letrec int f(x:int) = if zero?(x) then 0\
                                     \else -((f -(x,1)), m) in (f 4)" "int"
  , PassCheck "HO-nested-letrecs"
            "letrec (int->int) even(odd:(int->int)) = proc(x:int) if zero?(x) then 1 else (odd -(x,1))\
            \in letrec int odd(x:int) = if zero?(x) then 0 else ((even odd) -(x,1))\
               \in (odd 13)" "int"
  ]

testList :: [Test]
testList = [
    NumTest "positive-const" "11" 11
  , NumTest "negative-const" "-33" (-33)
  , NumTest "simple-arith-1" "-(44,33)" 11
  , NumTest "nested-arith-left" "-(-(44,33),22)" (-11)
  , NumTest "nested-arith-right" "-(55, -(22,11))" 44
  , NumTest "test-var-1" "x" 10
  , NumTest "test-var-2" "-(x,1)" 9
  , NumTest "test-var-3" "-(1,x)" (-9)
  , ErrTest "test-unbound-var-1" "foo"
  , ErrTest "test-unbound-var-1" "-(x,foo)"
  , NumTest "if-true" "if zero?(0) then 3 else 4" 3
  , NumTest "if-false" "if zero?(1) then 3 else 4" 4
  , ErrTest "no-bool-to-diff-1" "-(zero?(0),1)"
  , ErrTest "no-bool-to-diff-2" "-(1,zero?(0))"
  , ErrTest "no-int-to-if" "if 1 then 2 else 3"
  , NumTest "if-eval-test-true" "if zero?(-(11,11)) then 3 else 4" 3
  , NumTest "if-eval-test-false" "if zero?(-(11, 12)) then 3 else 4" 4
  , ErrTest "if-eval-test-true-2" "if zero?(-(11, 11)) then 3 else foo"
  , ErrTest "if-eval-test-false-2" "if zero?(-(11,12)) then foo else 4"
  , NumTest "simple-let-1" "let x = 3 in x" 3
  , NumTest "eval-let-body" "let x = 3 in -(x,1)" 2
  , NumTest "eval-let-rhs" "let x = -(4,1) in -(x,1)" 2
  , NumTest "simple-nested-let" "let x = 3 in let y = 4 in -(x,y)" (-1)
  , NumTest "check-shadowing-in-body" "let x = 3 in let x = 4 in x" 4
  , NumTest "check-shadowing-in-rhs" "let x = 3 in let x = -(x,1) in x" 2
  , NumTest "apply-proc-in-rator-pos" "(proc (x:int) -(x,1)  30)" 29
  , NumTest "apply-simple-proc" "let f = proc (x:int) -(x,1) in (f 30)" 29
  , NumTest "let-to-proc-1" "(proc (f:(int->int)) (f 30)  proc (x:int) -(x,1))" 29
  , NumTest "nested-procs" "((proc (x:int) proc (y:int) -(x,y)  5) 6)" (-1)
  , NumTest "nested-procs2" "let f = proc(x:int) proc (y:int) -(x,y) in ((f -(10,5)) 6)" (-1)
  , NumTest "scope-of-closure" "let f = let y = 5 in proc (x:int) -(x, -(0, y)) in (f 7)" 12
  , NumTest "simple-letrec-1" "letrec int f(x:int) = -(x,1) in (f 33)" 32
  , NumTest "simple-letrec-2"
            "letrec int f(x:int) = if zero?(x) then 0 else -((f -(x,1)), -2) in (f 4)"
            8
  , NumTest "simple-letrec-3"
            "let m = -5\
            \in letrec int f(x:int) = if zero?(x) then 0\
                                     \else -((f -(x,1)), m) in (f 4)" 20
  , NumTest "HO-nested-letrecs"
            "letrec (int->int) even(odd:(int->int)) = proc(x:int) if zero?(x) then 1 else (odd -(x,1))\
            \in letrec int odd(x:int) = if zero?(x) then 0 else ((even odd) -(x,1))\
               \in (odd 13)" 1
  ]

