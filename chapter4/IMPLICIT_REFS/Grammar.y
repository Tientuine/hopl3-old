{
module IMPLICIT_REFS.Parser where

import Data.Char
import IMPLICIT_REFS.AST
import IMPLICIT_REFS.Scanner
}

%name calc
%tokentype { Token }
%error { parseError }

%token
    Num         { TNum $$ }
    Id          { TVar $$ }
    "let"       { TLet }
    "in"        { TIn }
    "if"        { TIf }
    "then"      { TThen }
    "else"      { TElse }
    "zero?"     { TIsZero }
    "proc"      { TProc }
    "letrec"    { TLetRec }
    "begin"     { TBegin }
    "end"       { TEnd }
    "set"       { TSetref }
    "="         { TAssign }
    "-"         { TMinus }
    ","         { TComma }
    ";"         { TSemicolon }
    "("         { TLParen }
    ")"         { TRParen }

%%

{- GRAMMATICAL PRODUCTIONS -}

Pgm : Exp { Pgm $1 }

Exp : Num { ConstExp $1 }
    | Id  { VarExp $1 }
    | "zero?" "(" Exp ")"             { IsZeroExp $3 }
    | "-" "(" Exp "," Exp ")"         { DiffExp   $3 $5 }
    | "if" Exp "then" Exp "else" Exp  { IfExp     $2 $4 $6 }
    | "let" Id "=" Exp "in" Exp       { LetExp    $2 $4 $6 }
    | "proc" "(" Id ")" Exp           { ProcExp   $3 $5 }
    | "(" Exp Exp ")"                 { CallExp   $2 $3 }
    | "letrec" Id "(" Id ")" "=" Exp "in" Exp
                                      { LetrecExp $2 $4 $7 $9 }
    | "begin" ExpSeq "end"            { BeginExp  $2 }
    | "set" Id "=" Exp                { AssignExp $2 $4 }

ExpSeq : Exp { [$1] }
       | Exp ";" ExpSeq { $1 : $3 }

{

parseError :: [Token] -> a
parseError xs = error $ "Parse error " ++ show xs

parse = calc . lexer

}

