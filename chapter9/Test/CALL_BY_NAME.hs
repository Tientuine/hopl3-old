{-
 - Test/CALL_BY_NAME.hs
 -
 - Test cases for the CALL_BY_NAME language.
 -}
module Test.CALL_BY_NAME where

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
                  \in ((p b) (q b))" 44
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
  , NumTest "lazy-infinite-loop"
            "letrec infinite-loop (x) = (infinite-loop -(x,-1)) \
            \in let f = proc (z) 11 \
            \in (f (infinite-loop 0))" 11
  , NumTest "lazy-makerec"
            "let makerec = proc (f) \
                              \let d = proc (x) (f (x x)) \
                              \in (f (d d)) \
            \in let maketimes4 = proc (f) proc (x) \
                                    \if zero?(x) \
                                    \then 0 \
                                    \else -((f -(x,1)), -4) \
               \in let times4 = (makerec maketimes4) \
                  \in (times4 3)"  12
  ]

