{-
 -  CLASSES/AST.hs
 -
 -  Reference implementation of the toy language CLASSES by Mitchell Wand
 -  This module provides types for representing the abstract syntax tree.
 -
 -  Author: Matthew A Johnson
 -}
module CLASSES.AST where

import           Common.Types    (Id)

{- Parse types -}

-- The AST node for a program contains class declarations and an expression
data Pgm = Pgm [ClassDecl] Exp deriving Show

-- Abstract syntax for class declarations
data ClassDecl  = ClassDecl Id Id [Id] [MethodDecl] deriving Show

-- Abstract syntax for method declarations
data MethodDecl = MethodDecl Id [Id] Exp deriving Show

-- There are many varieties of AST node for expressions
data Exp
    = ConstExp Int
    | VarExp Id
    | DiffExp Exp Exp
    | IsZeroExp Exp
    | IfExp Exp Exp Exp
    | LetExp [Id] [Exp] Exp
    | ProcExp Id Exp
    | CallExp Exp Exp
    | IsNegExp Exp
    | LetrecExp Id Id Exp Exp
    | BeginExp [Exp]
    | AssignExp Id Exp
    | EmptyExp
    | IsNullExp Exp
    | ConsExp Exp Exp
    | CarExp Exp
    | CdrExp Exp
    | ListExp [Exp]
    | NewObjExp Id [Exp]
    | MethodCallExp Exp Id [Exp]
    | SuperCallExp Id [Exp]
    | SelfExp
    | InstanceofExp Exp Id
  deriving (Eq,Show)

