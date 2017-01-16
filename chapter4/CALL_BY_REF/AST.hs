{-
 -  CALL_BY_REF/AST.hs
 -
 -  Reference implementation of the toy language CALL_BY_REF by Mitchell
 -  Wand. This module provides types for representing abstract syntax trees.
 -
 -  Author: Matthew A Johnson
 -}
module CALL_BY_REF.AST where

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
    | AssignExp Id  Exp
    | BeginExp [Exp]
    | NewPairExp  Exp Exp
    | LeftExp     Exp
    | RightExp    Exp
    | SetLeftExp  Exp Exp
    | SetRightExp Exp Exp
  deriving (Eq,Show)

