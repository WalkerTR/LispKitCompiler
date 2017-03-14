module Compiler(
    comp_one,
    Secdexpr(..)
) where

import Parser2



data Secdexpr = Add | Sub |  Mult | Div | Rem | Eq | Leq |
                Car | Cdr | Cons | Atom | Join | Rtn | Stop | Push |
                Ap | Rap | Ld (Integer, Integer) |
                Ldc LKC|
                Sel [Secdexpr] [Secdexpr]|
                Ldf [Secdexpr]
                deriving(Show, Eq)

position::String -> [LKC]-> Integer
position x [] = error "position: variable not found"
position x ((VAR z):y) = if z==x then 0 else 1 + (position x y)
position x _ = error "position: found not VAR"

member::String ->[LKC]->Bool
member x  [] = False
member x ((VAR z):y) = if x == z then True else member x y
member x _ = error ("member: trovata non VAR"++ x)

location:: String->Integer-> [[LKC]]-> (Integer, Integer)
location x _ []= error ("location non trova VAR"++ x)
location x ct (n:m) =   if (member x n) then (ct, (position x n)) else  (location x (ct+1) m)


sexpr_reverse::[a]->[a]
sexpr_reverse []= []
sexpr_reverse (a:b)= (sexpr_reverse b) ++ [a]


vars::[(a,b)]->[a]
vars []= []
vars ((x,y):r)= (x : (vars r))

exprs::[(a,b)]->[b]
exprs []= []
exprs((x,y):r)= (y:(exprs r))

complist:: [LKC]-> [[LKC]] -> [Secdexpr]->[Secdexpr]
complist [] _ c = ((Ldc NIL):c)
complist (x:y) n c = complist y n (comp x n (Cons:c))


comp:: LKC -> [[LKC]] -> [Secdexpr]->[Secdexpr]
comp e n c =  case e of (VAR x) -> ((Ld (location x 0 n)):c)
                        (NUM x)-> (Ldc (NUM x)):c
                        (BOO x)-> (Ldc (BOO x)):c
                        (STRI x)-> (Ldc (STRI x)):c
                        NIL -> (Ldc NIL):c
                        (ADD x y)-> comp y n (comp x n (Add:c))
                        (SUB x y)-> comp y n (comp x n (Sub:c))
                        (MULT x y)-> comp y n (comp x n (Mult:c))
                        (DIV x y)-> comp y n (comp x n (Div:c))
                        (REM x y)-> comp y n (comp x n (Rem:c))
                        (EQC x y)-> comp y n (comp x n (Eq:c))
                        (LEQC x y)-> comp y n (comp x n (Leq:c))
                        (CARC x)-> comp x n (Car:c)
                        (CDRC x)-> comp x n (Cdr:c)
                        (CONSC x y)-> comp y n (comp x n (Cons:c))
                        (ATOMC x)-> comp x n (Atom:c)
                        (IFC x y z)-> let thenp=(comp y n [Join])
                                          elsep=(comp z n [Join])
                                      in comp x n  ((Sel thenp elsep):c)
                        (LAMBDAC x y)-> (Ldf (comp y (x:n) [Rtn])):c
                        (LETC x y)-> let
                                        binders = map fst y
                                        params = map snd y
                                     in complist params n ((Ldf (comp x (binders:n) [Rtn])):Ap:c)
                        (LETRECC x y)-> let
                                        binders = map fst y
                                        params = map snd y
                                     in Push:(complist params (binders:n) ((Ldf (comp x (binders:n) [Rtn])):Rap:c))
                        (CALL x y)-> complist y n (comp x n (Ap:c))
                        _ -> [];


comp_one x = comp x [] []
