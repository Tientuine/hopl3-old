{
module CLASSES.Parser where

import Data.Char
import CLASSES.AST
import CLASSES.Scanner
}

%name calc
%tokentype { Token }
%error { parseError }

%token
    Num          { TNum $$ }
    Id           { TVar $$ }
    "let"        { TLet }
    "in"         { TIn }
    "if"         { TIf }
    "then"       { TThen }
    "else"       { TElse }
    "zero?"      { TIsZero }
    "proc"       { TProc }
    "neg?"       { TIsNeg }
    "letrec"     { TLetRec }
    "begin"      { TBegin }
    "end"        { TEnd }
    "set"        { TSetref }
    "emptylist"  { TEmpty }
    "null?"      { TIsNull }
    "cons"       { TCons }
    "car"        { TCar }
    "cdr"        { TCdr }
    "list"       { TList }
    "class"      { TClass }
    "extends"    { TExtends }
    "field"      { TField }
    "method"     { TMethod }
    "new"        { TNew }
    "send"       { TSend }
    "super"      { TSuper }
    "self"       { TSelf }
    "instanceof" { TInstanceof }
    "="          { TAssign }
    "-"          { TMinus }
    ","          { TComma }
    ";"          { TSemicolon }
    "("          { TLParen }
    ")"          { TRParen }

%%

Pgm : ClassDecls Exp   { Pgm $1 $2 }

ClassDecl : "class" Id "extends" Id Fields MethodDecls { ClassDecl $2 $4 $5 $6 }

MethodDecl : "method" Id "(" ParamList ")" Exp { MethodDecl $2 $4 $6 }

Field : "field" Id { $2 }

Exp : Num   { ConstExp $1 }
    | Id    { VarExp $1 }
    | "-" "(" Exp "," Exp ")"     { DiffExp $3 $5 }
    | "zero?" "(" Exp ")"         { IsZeroExp $3 }
    | "if" Exp "then" Exp "else" Exp  { IfExp $2 $4 $6 }
    | "let" VarDecls "in" Exp     { LetExp (fst $2) (snd $2) $4 }
    | "proc" "(" Id ")" Exp       { ProcExp $3 $5 }
    | "(" Exp Exp ")"             { CallExp $2 $3 }
    | "neg?" "(" Exp ")"          { IsNegExp $3 }
    | "letrec" Id  "(" Id ")" "=" Exp "in" Exp { LetrecExp $2 $4 $7 $9 }
    | "begin" ExpSeq "end"        { BeginExp $2 }
    | "set" Id "=" Exp            { AssignExp $2 $4 }
    | "emptylist"                 { EmptyExp }
    | "null?" "(" Exp ")"         { IsNullExp $3 }
    | "cons" "(" Exp "," Exp ")"  { ConsExp $3 $5 }
    | "car"  "(" Exp ")"          { CarExp $3 }
    | "cdr"  "(" Exp ")"          { CdrExp $3 }
    | "list" "(" ExpList ")"      { ListExp $3 }
    | "new" Id "(" ExpList ")"    { NewObjExp $2 $4 }
    | "send" Exp Id "(" ExpList ")" { MethodCallExp $2 $3 $5 }
    | "super" Id "(" ExpList ")"  { SuperCallExp $2 $4 }
    | "self"                      { SelfExp }
    | "instanceof" Exp Id         { InstanceofExp $2 $3 }

ClassDecls : {-empty-}            { [] }
           | ClassDecl ClassDecls { $1:$2 }

MethodDecls : {-empty-}              { [] }
            | MethodDecl MethodDecls { $1:$2 }

VarDecls : Id "=" Exp          { ([$1],[$3]) }
         | Id "=" Exp VarDecls { ($1:fst $4,$3:snd $4) }

Fields : {-empty-}    { [] }
       | Field Fields { $1:$2 }

ParamList : {-empty-}         { [] }
          | NonEmptyParamList { $1 }

NonEmptyParamList : Id        { [$1] }
                  | Id "," NonEmptyParamList { $1:$3 }

ExpList : {-empty-}       { [] }
        | NonEmptyExpList { $1 }

NonEmptyExpList : Exp     { [$1] }
                | Exp "," NonEmptyExpList { $1:$3 }

ExpSeq : Exp { [$1] }
       | Exp ";" ExpSeq { $1 : $3 }

{

{-
-}

parseError :: [Token] -> a
parseError xs = error $ "Parse error "++(show xs)

parser = calc . lexer

}

