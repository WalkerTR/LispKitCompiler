module Lexer (
Token(..),
Symbol_T(..),
Operator_T(..),
Keyword_T(..),
lexi
) where

import Prelude hiding (EQ)

data Keyword_T = LET | IN | END | LETREC | AND | IF | THEN | ELSE | LAMBDA
    deriving (Show,Eq)

data Operator_T = EQ | LEQ | CAR | CDR | CONS | ATOM
    deriving (Show,Eq)

data Symbol_T = LPAREN | RPAREN | EQUALS | PLUS | MINUS | TIMES | DIVISION | COMMA | DOLLAR
    deriving (Show,Eq)

data Token = Keyword Keyword_T | Operator Operator_T | Id String |
    Symbol Symbol_T | Number Integer | String String | Bool Bool | Nil
    deriving (Show,Eq)


isAlphaChar c = c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'])
isDigitChar c = c `elem` ['0' .. '9']
isIdChar c = isAlphaChar c || isDigitChar c
isSeparator c = c `elem` "()=$,"
isSpace c = c `elem` [' ', '\n', '\f', '\r', '\t']
isSymbol c = c `elem` "()=+-*/,"

extractWord :: String -> Token
extractWord w = case w of
    "let"     -> Keyword LET
    "in"      -> Keyword IN
    "end"     -> Keyword END
    "letrec"  -> Keyword LETREC
    "and"     -> Keyword AND
    "if"      -> Keyword IF
    "then"    -> Keyword THEN
    "else"    -> Keyword ELSE
    "lambda"  -> Keyword LAMBDA

    "eq"      -> Operator EQ
    "leq"     -> Operator LEQ
    "car"     -> Operator CAR
    "cdr"     -> Operator CDR
    "cons"    -> Operator CONS
    "atom"    -> Operator ATOM

    "true"    -> Bool True
    "false"   -> Bool False

    "nil"     -> Nil

    otherwise -> Id w

toSymbol :: Char -> Symbol_T
toSymbol c = case c of
    '(' -> LPAREN
    ')' -> RPAREN
    '+' -> PLUS
    '-' -> MINUS
    '*' -> TIMES
    '/' -> DIVISION
    '=' -> EQUALS
    ',' -> COMMA



n :: String -> Integer -> Bool -> (Token, String)
n "" _ _ = error "Unexpected end of string"
n input@(c:l) num sign
    | isDigitChar c =
        let d = read [c] :: Integer
        in n l (num*10 + d) sign
    | otherwise = (Number((if sign then -1 else 1) * num), input)

sc :: String -> String -> (Token, String)
sc "" _ = error "Unexpected end of string"
sc ('"':l) res = (String res, l)
sc (c:l) res = sc l (res ++ [c])

s :: String -> String -> (Token, String)
s "" _ = error "Unexpected end of string"
s input@(c:l) res
    | isIdChar c = s l (res ++ [c])
    | otherwise = (extractWord(res), input)


i :: String -> [Token]
i "" = error "Unexpected end of string"
i "$" = [(Symbol DOLLAR)]
i (' ':l) = i l
i input@(f:l)
    | isSpace f         = i l
    | f == '"'          = let (t, str) = sc l "" in t:(i str)
    | f == '~'          = let (t, str) = n l 0 True in  if null l then
                                                                    error "Unexpected end of string"
                                                                else if isDigitChar (head l) then
                                                                    t:(i str)
                                                                else
                                                                    error "Unexpected character"
    | isDigitChar f     = let (t, str) = n input 0 False in t:(i str)
    | isIdChar f     = let (t, str) = s input "" in t:(i str)
    | isSymbol f        = (Symbol (toSymbol f)):(i l)
    | otherwise         = error "Unexpected character"


lexi :: String -> [Token]
lexi = i
