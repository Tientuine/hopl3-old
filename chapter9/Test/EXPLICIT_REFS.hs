{-
 - Test/EXPLICIT_REFS.hs
 -
 - Test cases for the EXPLICIT_REFS language.
 -}
module Test.EXPLICIT_REFS where

import           Test.Base
import qualified Test.LETREC as LETREC

testList :: [Test]
testList = LETREC.testList ++ [
    NumTest "begin-test-1" "begin 1; 2; 3 end" 3
  , NumTest "gensym-test-1" "let g = let counter = newref(0)\
                                    \in proc (dummy) let d = setref(counter, -(deref(counter),-1))\
                                                      \in deref(counter)\
                            \in -((g 11),(g 22))" (-1)
  , NumTest "simple-store-test-1" "let x = newref(17) in deref(x)" 17
  , NumTest "assignment-test-1" "let x = newref(17)\
                                \in begin setref(x,27); deref(x) end" 27
  , NumTest "gensym-test-2" "let g = let counter = newref(0)\
                                     \in proc (dummy) begin \
                                                      \setref(counter, -(deref(counter),-1));\
                                                      \deref(counter)\
                                                      \end \
                             \in -((g 11),(g 22))" (-1)
{-, NumTest "even-odd-via-set-1" "\
        \let x = newref(0)\
        \in letrec even(d) = if zero?(deref(x))\
                            \then 1 \
                            \else let d = setref(x, -(deref(x),1))\
                                 \in (odd d)\
                  \odd(d)  = if zero?(deref(x))\
                            \then 0 \
                            \else let d = setref(x, -(deref(x),1))\
                                 \in (even d)\
            \in let d = setref(x,13) in (odd -100)" 1-}
  , NumTest "show-allocation-1" "let x = newref(22)\
                                \in let f = proc (z) let zz = newref(-(z,deref(x))) \
                                                    \in deref(zz) \
                                   \in -((f 66), (f 55))" 11
  , NumTest "chains-1" "let x = newref(newref(0))\
                        \in begin setref(deref(x), 11);\
                                 \deref(deref(x))\
                           \end" 11
  ]

