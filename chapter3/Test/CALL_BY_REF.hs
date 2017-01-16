{-
 - Test/CALL_BY_REF.hs
 -
 - Test cases for the CALL_BY_REF language.
 -}
module Test.CALL_BY_REF where

import           Test.Base
import qualified Test.MUTABLE_PAIRS as MUTABLE_PAIRS

testList :: [Test]
testList = MUTABLE_PAIRS.testList ++ [
    NumTest "cbr-swap-1"
            "let swap = proc (x) proc (y) \
                                    \let temp = x \
                                    \in begin \
                                            \set x = y; \
                                            \set y = temp \
                                       \end \
            \in let a = 33 \
               \in let b = 44 \
                  \in begin \
                         \ ((swap a) b); \
                         \-(a,b) \
                     \end" 11
  , NumTest "cbr-global-aliasing-1"
            "let p = proc (z) set z = 44 \
            \in let x = 33 \
               \in begin (p x); x end" 44
  , NumTest "cbr-direct-aliasing-1"
            "let p = proc (x) proc (y) \
                                 \begin \
                                     \set x = 44; \
                                     \y \
                                 \end \
            \in let b = 33 \
               \in ((p b) b)" 44
    -- in this language, you can't return a reference.
  , NumTest "cbr-indirect-aliasing-1"
            "let p = proc (x) proc (y) \
                                 \begin \
                                     \set x = 44; \
                                     \y \
                                 \end \
            \in let q = proc(z) z \
               \in let b = 33 \
                  \in ((p b) (q b))" 33
    -- in this language, you can't return a reference.
  , NumTest "cbr-indirect-aliasing-2"
            "let p = proc (x) proc (y) \
                                 \begin \
                                     \set x = 44; \
                                     \y \
                                 \end \
            \in let q = proc(z) z \
               \in let b = 33 \
                  \in ((p (q b)) b)" 33
  , NumTest "cbr-sideeffect-a-passed-structure-1"
            "let f = proc (x) setleft x = -(left(x),-1) \
            \in let p = newpair (44,newpair(55,66)) \
               \in begin \
                      \ (f right(p)); \
                      \left(right(p)) \
                  \end" 56
  , NumTest "cbr-example-for-book"
            "let f = proc (x) set x = 44 \
            \in let g = proc (y) (f y) \
               \in let z = 55 \
                  \in begin \
                         \ (g z); \
                         \z \
                     \end" 44
  ]

