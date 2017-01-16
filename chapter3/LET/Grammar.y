{
module LET.Parser (parse) where

import Data.Char
import LET.AST
import LET.Scanner
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

{

parseError :: [Token] -> a
parseError xs = error $ "Parse error " ++ show xs

-- Exported parser includes lexical analyzer front-end
parse = calc . lexer

}

