{-
 - Test/IMPLICIT_REFS.hs
 -
 - Test cases for the IMPLICIT_REFS language.
 -}
module Test.IMPLICIT_REFS where

import           Test.Base
import qualified Test.LETREC as LETREC

testList :: [Test]
testList = LETREC.testList ++ [
    NumTest "begin-test-1" "begin 1; 2; 3 end" 3
  , NumTest "gensym-test" "let g = let count = 0 \
                                  \in proc(d) let d = set count = -(count,-1) \
                                             \in count \
                          \in -((g 11), (g 22))" (-1)
  , NumTest "assignment-test-1" "let x = 17 \
                                \in begin set x = 27; x end" 27
{-, NumTest "even-odd-via-set" "let x = 0 \
                               \in letrec even(d) = if zero?(x) then 1 \
                                                   \else let d = set x = -(x,1) \
                                                        \in (odd d) \
                                         \odd(d)  = if zero?(x) then 0 \
                                                   \else let d = set x = -(x,1) \
                                                        \in (even d) \
                                   \in let d = set x = 13 in (odd -99)" 1-}
  , NumTest "example-for-book-1" "let f = proc (x) proc (y) \
                                                    \begin \
                                                    \set x = -(x,-1); \
                                                    \-(x,y) \
                                                    \end \
                                 \in ((f 44) 33)" 12
    ]

