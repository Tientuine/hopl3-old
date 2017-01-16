{-
 -  CLASSES/Store.hs
 -
 -  Reference implementation of the toy language CLASSES by Mitchell
 -  Wand. This module provides a simple abstraction of a persistent free store.
 -  Memory locations are simply elements of a list, and references are integer
 -  indexes for looking up those elements.
 -
 -  Author: Matthew A Johnson
 -}
module CLASSES.Store (
    Store,Reference,freestore,newref,newrefs,deref,setref,unref,unrefs
) where

import           Data.List

import {-# SOURCE #-} CLASSES.Val    (StoVal)

type Store     = FreeStore StoVal
type Size      = Int
type Reference = Int

newtype FreeStore v = FreeStore (Size,[v],[Reference]) deriving Show

-- Free store represented as a list

freestore :: Int -> v -> FreeStore v
freestore n v = FreeStore (n,replicate n v,[])

newref :: v -> FreeStore v -> (Reference,FreeStore v)
newref val (FreeStore (n,locs,refs)) = (ref,σ')
  where
    ref   = nextRef refs n
    refs' = ref:refs
    σ'    = setref ref val (FreeStore (n,locs,refs'))

newrefs :: [v] -> FreeStore v -> ([Reference],FreeStore v)
newrefs vals σ@(FreeStore (n,locs,refs)) = (refs',σ'')
  where
    ref   = nextBlock refs n k
    refs' = [ref..ref+k-1]
    σ''   = foldr (\(ref,val) σ' -> setref ref val σ')
                (FreeStore (n,locs,refs'++refs)) (zip refs' vals)
    k     = length vals

deref :: Reference -> FreeStore v -> v
deref ref (FreeStore (n,locs,refs))
    | ref `elem` refs = locs !! ref
    | otherwise  = error $ "Store exception: deref access violation (" ++ show ref ++ "," ++ show refs ++ ")"

setref :: Reference -> v -> FreeStore v -> FreeStore v
setref ref val (FreeStore (n,locs,refs))
    | ref `elem` refs = FreeStore (n,locs',refs)
    | otherwise  = error $ "Store exception: setref access violation (" ++ show ref ++ "," ++ show refs ++ ")"
  where
    locs' = take ref locs ++ (val : drop (ref+1) locs)

{-cleanup :: [Reference] -> Store v -> Store v
cleanup rs (Store (n,locs,refs)) = Store (n,locs,refs')
    where refs' = filter (`elem` rs) refs-}

unref :: Reference -> FreeStore v -> FreeStore v
unref r (FreeStore (n,locs,refs)) = FreeStore (n,locs,refs')
  where
    refs' = delete r refs

unrefs :: Reference -> Size -> FreeStore v -> FreeStore v
unrefs ref k (FreeStore (n,locs,refs)) = FreeStore (n,locs,refs')
  where
    refs' = refs \\ [ref..ref+k]

minfree :: [Int] -> Int
minfree xs = minfrom 0 (length xs,xs)

nextRef :: [Reference] -> Int -> Reference
nextRef refs n | ref < n   = ref
               | otherwise = error "Store exception: out of memory"
  where
    ref = minfree refs

nextBlock :: [Reference] -> Int -> Int -> Reference
nextBlock refs n k | ref + k <= n = ref
                   | otherwise    = error $ "Store exception: out of memory (" ++ show k ++ ")"
  where
    ref = blockfree k refs

blockfree :: Int -> [Int] -> Int
blockfree 1 xs = minfree xs
blockfree n xs = blockfrom 0 n xs

blockfrom :: Int -> Int -> [Int] -> Int
blockfrom i _ [] = i
blockfrom i n xs | null us'  = j
                 | otherwise = blockfrom (j+1) n (us'++vs)
  where
    j       = minfrom i (length xs,xs)
    (us,vs) = partition (\ x -> x < j+n) xs
    (us',_) = partition (\ x -> x > j)   us

minfrom :: Int -> (Int,[Int]) -> Int
minfrom a (n,xs) | n == 0     = a
                 | m == b - a = minfrom b (n-m,vs)
                 | otherwise = minfrom a (m,us)
  where
    (us,vs) = partition (< b) xs
    b       = a + 1 + n `div` 2
    m       = length us

