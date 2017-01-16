{
module CHECKED.Parser where

import Data.Char
import CHECKED.AST
import CHECKED.Scanner
import CHECKED.Type
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
    "int"       { TInt }
    "bool"      { TBool }
    "->"        { TArrow }
    ":"         { TColon }

%%

{- GRAMMATICAL PRODUCTIONS -}

Pgm : Exp { Pgm $1 }

Exp : Num { ConstExp $1 }
    | Id  { VarExp   $1 }
    | "zero?" "(" Exp ")"             { IsZeroExp $3 }
    | "-" "(" Exp "," Exp ")"         { DiffExp   $3 $5 }
    | "if" Exp "then" Exp "else" Exp  { IfExp     $2 $4 $6 }
    | "let" Id "=" Exp "in" Exp       { LetExp    $2 $4 $6 }
    | "proc" "(" Param ")" Exp        { ProcExp (fst $3) (snd $3) $5 }
    | "(" Exp Exp ")"                 { CallExp   $2 $3 }
    | "letrec" Type Id "(" Param ")" "=" Exp "in" Exp
                                      { LetrecExp $2 $3 (fst $5) (snd $5) $8 $10 }

Param : Id ":" Type { ($1,$3) }

Type : "int"  { IntType }
     | "bool" { BoolType }
     | "(" Type "->" Type ")" { ProcType $2 $4 }

{
	
parseError :: [Token] -> a
parseError xs = error $ "Parse error " ++ show xs

-- Exported parser includes lexical analyzer front-end
parse = calc . lexer

}

