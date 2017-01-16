{
module LETREC.Parser where

import Data.Char
import LETREC.AST
import LETREC.Scanner
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
    "="         { TAssign }
    "-"         { TMinus }
    ","         { TComma }
    "("         { TLParen }
    ")"         { TRParen }

%%

{- GRAMMATICAL PRODUCTIONS -}

Pgm : Exp { Pgm $1 }

Exp : Num { ConstExp $1 }
    | Id  { VarExp   $1 }
    | "zero?" "(" Exp ")"             { IsZeroExp $3 }
    | "-" "(" Exp "," Exp ")"         { DiffExp   $3 $5 }
    | "if" Exp "then" Exp "else" Exp  { IfExp     $2 $4 $6 }
    | "let" Id "=" Exp "in" Exp       { LetExp    $2 $4 $6 }
    | "proc" "(" Id ")" Exp           { ProcExp   $3 $5 }
    | "(" Exp Exp ")"                 { CallExp   $2 $3 }
    | "letrec" Id  "(" Id ")" "=" Exp "in" Exp
                                      { LetrecExp $2 $4 $7 $9 }

{

parseError :: [Token] -> a
parseError xs = error $ "Parse error " ++ show xs

-- Exported parser includes lexical analyzer front-end
parse = calc . lexer

}

