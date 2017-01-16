{-
 - Test/LET.hs
 -
 - Test cases for the LET language.
 -}
module Test.LET where

import           Test.Base

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
  , NumTest "if-eval-test-true-2" "if zero?(-(11, 11)) then 3 else foo" 3
  , NumTest "if-eval-test-false-2" "if zero?(-(11,12)) then foo else 4" 4
  , NumTest "simple-let-1" "let x = 3 in x" 3
  , NumTest "eval-let-body" "let x = 3 in -(x,1)" 2
  , NumTest "eval-let-rhs" "let x = -(4,1) in -(x,1)" 2
  , NumTest "simple-nested-let" "let x = 3 in let y = 4 in -(x,y)" (-1)
  , NumTest "check-shadowing-in-body" "let x = 3 in let x = 4 in x" 4
  , NumTest "check-shadowing-in-rhs" "let x = 3 in let x = -(x,1) in x" 2
  ]

