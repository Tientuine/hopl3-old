{
module PROC.Scanner (lexer, Token(..)) where

}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$graphic    = $printable # $white
$comment    = $printable # [\n]

@char       = \' ($graphic # \')* \'
@string     = \" ($graphic # \")* \"

tokens :-
    $white+               ;
    "%" $comment+         ;
    "let"                 { \s -> TLet }
    "in"                  { \s -> TIn }
    "if"                  { \s -> TIf }
    "then"                { \s -> TThen }
    "else"                { \s -> TElse }
    "zero?"               { \s -> TIsZero }
    "proc"                { \s -> TProc }
    "="                   { \s -> TAssign }
    "("                   { \s -> TLParen }
    ")"                   { \s -> TRParen }
    "-"                   { \s -> TMinus }
    ","                   { \s -> TComma }
    "-"?$digit+           { \s -> TNum (read s) }
    $alpha[$alpha $digit \_ \- \? \']*  { \s -> TVar s }
{

-- The token type:
data Token
    = TComma
    | TLet
    | TIn
    | TIf
    | TThen
    | TElse
    | TAssign
    | TLParen
    | TRParen
    | TMinus
    | TIsZero
    | TProc
    | TVar String
    | TNum Int
    | TEOF
  deriving (Eq,Show)

-- Each action has type :: String -> Token

lexer = alexScanTokens

}

