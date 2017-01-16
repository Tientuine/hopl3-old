{-
 -  CHECKED/Type.hs
 -
 -  Reference implementation of the toy language CHECKED by Mitchell Wand.
 -  This module provides a Haskell ADT for representing type information.
 -
 -  Author: Matthew A Johnson
 -}
module CHECKED.Type where

data Type
    = NoType String
    | IntType
    | BoolType
    | ProcType Type Type
  deriving (Eq)

instance Show Type where
    show (NoType s) = s
    show IntType    = "int"
    show BoolType   = "bool"
    show (ProcType targ tres) = "(" ++ show targ ++ " -> " ++ show tres ++ ")"

