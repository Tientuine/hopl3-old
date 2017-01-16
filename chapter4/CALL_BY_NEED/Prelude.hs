{-
 -  CALL_BY_NEED/Prelude.hs
 -
 -  Reference implementation of the toy language CALL_BY_NEED by Mitchell
 -  Wand. This module provides a standard library of pre-defined functions.
 -
 -  Author: Matthew A Johnson
 -}
module CALL_BY_NEED.Prelude where

import           Common.Types

type Def = (Id,Source)

preludeSrc :: [Def]
preludeSrc = [
    {- Insert new definitions here at the head of the list -}
    ("twice"  , "proc (x) -(x,-(0,x))")
  , ("incr"   , "proc (x) -(x,-(0,1))")
  , ("decr"   , "proc (x) -(x,1)")
  ]

