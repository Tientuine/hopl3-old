{-
 -  IMPLICIT_REFS/Store.hs
 -
 -  Reference implementation of the toy language IMPLICIT_REFS by Mitchell
 -  Wand. This module provides a simple abstraction of a persistent free store.
 -  Memory locations are simply elements of a list, and references are integer
 -  indexes for looking up those elements.
 -
 -  Author: Matthew A Johnson
 -}
module IMPLICIT_REFS.Store (
    Store,Reference,emptyStore,newref,deref,setref
  ) where

import           IMPLICIT_REFS.Val    (StoVal)

type Reference = Int
type Store     = [StoVal]

emptyStore :: Store
emptyStore = []

-- Free store represented as a list
newref :: StoVal -> Store -> (Reference,Store)
newref val store = (length store, store ++ [val])

deref :: Reference -> Store -> StoVal
deref ref store = store !! ref

setref :: Reference -> StoVal -> Store -> Store
setref ref val store = take ref store ++ (val : drop (ref+1) store)

