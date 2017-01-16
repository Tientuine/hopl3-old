{
module CLASSES.Scanner (lexer, Token(..)) where

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
    "neg?"                { \s -> TIsNeg }
    "letrec"              { \s -> TLetRec }
    "begin"               { \s -> TBegin }
    "end"                 { \s -> TEnd }
    "set"                 { \s -> TSetref }
    "emptylist"           { \s -> TEmpty }
    "null?"               { \s -> TIsNull }
    "cons"                { \s -> TCons }
    "car"                 { \s -> TCar }
    "cdr"                 { \s -> TCdr }
    "list"                { \s -> TList }
    "class"               { \s -> TClass }
    "extends"             { \s -> TExtends }
    "field"               { \s -> TField }
    "method"              { \s -> TMethod }
    "new"                 { \s -> TNew }
    "send"                { \s -> TSend }
    "super"               { \s -> TSuper }
    "self"                { \s -> TSelf }
    "instanceof"          { \s -> TInstanceof }
    "="                   { \s -> TAssign }
    "("                   { \s -> TLParen }
    ")"                   { \s -> TRParen }
    "-"                   { \s -> TMinus }
    ","                   { \s -> TComma }
    ";"                   { \s -> TSemicolon }
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
    | TIsNeg
    | TLetRec
    | TBegin
    | TEnd
    | TSemicolon
    | TSetref
    | TEmpty
    | TIsNull
    | TCons
    | TCar
    | TCdr
    | TList
    | TClass
    | TExtends
    | TField
    | TMethod
    | TNew
    | TSend
    | TSuper
    | TSelf
    | TInstanceof
    | TVar String
    | TNum Int
    | TEOF
  deriving (Eq,Show)

-- Each action has type :: String -> Token

lexer = alexScanTokens

}

