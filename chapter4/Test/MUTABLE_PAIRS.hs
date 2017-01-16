{-
 - Test/MUTABLE_PAIRS.hs
 -
 - Test cases for the MUTABLE_PAIRS language.
 -}
module Test.MUTABLE_PAIRS where

import           Test.Base
import qualified Test.IMPLICIT_REFS as IMPLICIT_REFS

testList :: [Test]
testList = IMPLICIT_REFS.testList ++ [
    NumTest "simple-mutpair-left-1" "let p = newpair(22,33) in left(p)" 22
  , NumTest "simple-mutpair-right-1" "let p = newpair(22,33) in right(p)" 33
  , NumTest "simple-mutpair-setleft-1" "let p = newpair(22,33) \
                                       \in begin setleft p = 77; \
                                                \left(p) \
                                          \end" 77
  , NumTest "simple-mutpair-setleft-2" "let p = newpair(22,33) \
                                       \in begin setleft p = 77; \
                                                \right(p) \
                                          \end" 33
  , NumTest "simple-mutpair-setright-1" "let p = newpair(22,33) \
                                        \in begin setright p = 77; \
                                                 \right(p) \
                                           \end" 77

  , NumTest "simple-mutpair-setright-2" "let p = newpair(22,33) \
                                        \in begin setright p = 77; \
                                                 \left(p) \
                                           \end" 22
  , NumTest "gensym-using-mutable-pair-left"
            "let g = let count = newpair(0,0) \
                    \in proc (dummy) \
                            \begin \
                                \setleft count = -(left(count), -1); \
                                \left(count) \
                            \end \
            \in -((g 22), (g 22))" (-1)
  , NumTest "gensym-using-mutable-pair-right"
            "let g = let count = newpair(0,0) \
                    \in proc (dummy) \
                            \begin \
                                \setright count = -(right(count), -1); \
                                \right(count) \
                            \end \
            \in -((g 22), (g 22))" (-1)
  , NumTest "example-for-mutable-pairs-section"
            "let glo = newpair(11,22) \
            \in let f = proc (loc) \
                            \begin  % this is a comment\n \
                                \setright loc = left(loc); \
                                \setleft  glo = 99; \
                                \-(left(loc),right(loc)) \
                            \end \
                \in (f glo)" 88
  ]

