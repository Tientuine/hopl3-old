{-
 -  LETREC/AST.hs
 -
 -  Reference implementation of the toy language LETREC by Mitchell Wand.
 -  This module provides types for representing the abstract syntax tree.
 -
 -  Author: Matthew A Johnson
 -}
module LETREC.AST where

import           Common.Types    (Id)

{- Parse types -}

-- The AST node for a program contains an expression
data Pgm = Pgm Exp deriving (Eq,Show)

-- There are several types of AST node for expressions
data Exp
    = ConstExp  Int
    | VarExp    Id
    | IsZeroExp Exp
    | DiffExp   Exp Exp
    | IfExp     Exp Exp Exp
    | LetExp    Id  Exp Exp
    | ProcExp   Id  Exp
    | CallExp   Exp Exp
    | LetrecExp Id  Id  Exp Exp
  deriving (Eq,Show)

