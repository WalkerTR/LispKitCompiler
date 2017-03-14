module Interpreter(
    interpreter,
    interpr,
    Valore(..)
) where

import Compiler
import Parser2

data Valore = V  LKC| OGA |
              CLO [Secdexpr] [[Valore]]|
              VLISTA [Valore]
              deriving(Show,Eq)

data Dump = CONTR  [Secdexpr] |
            TRIPLA [Valore][[Valore]][Secdexpr] | DUMMY
            deriving(Show,Eq)



lazyE::[Valore]-> [Valore]->[Valore]
lazyE [] _ = []
lazyE (a:b) c = ((lazyClo a c):(lazyE b c))

lazyClo:: Valore -> [Valore]->Valore
lazyClo (CLO a b) c = (CLO a ((lazyE c c):b))
lazyClo(V x) _= (V x)
lazyClo(VLISTA x) _= (VLISTA x)
lazyClo x _= error ("LazyClo: incompatible value" ++ (show x))


index::Integer ->[a]->a
index n  s= if n==0 then (head s) else (index (n-1) (tail s))

locate::(Integer, Integer)-> [[Valore]]->Valore
locate  (a,b)  e = (index b (index a e));

extract_int (V (NUM x)) = x
extract_int x = error ("extract_int: not int" ++ (show x))

vhd (VLISTA (a:b)) = a
vhd (VLISTA [])  = error "vhd: empty list"
vhd _ = error "vhd: not list"

vtl (VLISTA (a:b)) = VLISTA b
vtl (VLISTA [])  = error "vtl: empty list";
vtl _ = error "vtl: not list"

vatom (V k)= V (BOO True)
vatom _ = V (BOO False)

bool2s_espressione:: Bool ->LKC
bool2s_espressione b = if b then (BOO True) else (BOO False)

eqValore::Valore -> Valore -> Bool
eqValore a@(V _) b = (eqV a b)
eqValore a@(VLISTA _) b = (eqVLISTA a b)
eqValore a  b = error ("eqValore: not values"++ (show a) ++ (show b))

eqVLISTA::Valore -> Valore ->Bool
eqVLISTA (VLISTA []) (VLISTA [])= True
eqVLISTA (VLISTA(a:b)) (VLISTA (c:d)) = (eqValore a c) && (eqVLISTA (VLISTA b) (VLISTA d))
eqVLISTA _ _= False


eqV (V a) (V b)= a==b
eqV _ _= False


interpreter:: [Valore] -> [[Valore]]-> [Secdexpr]-> [Dump]-> Valore
interpreter s e c d = case (head c) of
                                      Ld(b, n) -> let x = (locate (b,n) e)  in (interpreter (x:s) e (tail c) d)
                                      (Ldc k) -> case k of  NIL -> (interpreter ((VLISTA []):s) e (tail c) d)
                                                            STRI str -> (interpreter ((V (STRI str)):s) e (tail c) d)
                                                            BOO val -> (interpreter ((V (BOO val)):s) e (tail c) d)
                                                            NUM n -> (interpreter ((V (NUM n)):s) e (tail c) d)
                                                            _ -> error (show(k))
                                      Add -> let operand1 = extract_int (head s)
                                                 operand2 = extract_int(head (tail s))
                                             in  (interpreter ((V(NUM (operand1 + operand2))):(tail (tail s))) e (tail c)  d)

                                      Sub -> let operand1 = extract_int (head s)
                                                 operand2 = extract_int(head (tail s))
                                             in  (interpreter ((V(NUM (operand1 - operand2))):(tail (tail s))) e (tail c)  d)
                                      Mult -> let operand1 = extract_int (head s)
                                                  operand2 = extract_int(head (tail s))
                                              in  (interpreter ((V(NUM (operand1 * operand2))):(tail (tail s))) e (tail c)  d)
                                      Div -> let operand1 = extract_int (head s)
                                                 operand2 = extract_int(head (tail s))
                                             in  (interpreter ((V(NUM (operand1 `div` operand2))):(tail (tail s))) e (tail c)  d)
                                      Rem -> let operand1 = extract_int (head s)
                                                 operand2 = extract_int(head (tail s))
                                             in  (interpreter ((V(NUM (operand1 `mod` operand2))):(tail (tail s))) e (tail c)  d)
                                      Leq -> let operand1 = extract_int (head s)
                                                 operand2 = extract_int(head (tail s))
                                             in  (interpreter ((V(bool2s_espressione (operand1 <= operand2))):(tail (tail s))) e (tail c)  d)
                                      Eq -> case s of
                                        (w1:w2:w3) -> (interpreter ((V (bool2s_espressione (eqValore w1 w2))):w3) e (tail c) d)
                                      Car -> (interpreter ((vhd(head s) ):(tail s)) e (tail c) d)
                                      Cdr -> (interpreter ((vtl(head s) ):(tail s)) e (tail c) d)
                                      Cons -> case head (tail s) of
                                        (VLISTA x)-> (interpreter  (VLISTA ((head s):x):(tail (tail s))) e (tail c) d)
                                      Atom ->  (interpreter ((vatom (head s)):(tail s)) e (tail c) d)
                                      Sel sl1 sl2 -> case head s of
                                        (V (BOO True)) -> (interpreter (tail s) e sl1 ((CONTR (tail c)):d))
                                        (V (BOO False)) -> (interpreter (tail s) e sl2 ((CONTR (tail c)):d))
                                      Join -> case (head d) of (CONTR c1) -> (interpreter s e c1 (tail d))
                                      Ldf sl -> (interpreter ((CLO sl e):s) e (tail c) d)
                                      Ap -> case (head s) of
                                        (CLO c1 e1) -> case (head (tail s)) of
                                          VLISTA x -> (interpreter [] (x:e1) c1 ((TRIPLA (tail(tail s)) e (tail c)):d))
                                      Rtn ->  case (head d) of (TRIPLA s1 e1 c1) -> (interpreter ((head s):s1) e1 c1 (tail d))
                                      Rap -> case (head s) of
                                        (CLO c1 e1) ->  case e1 of
                                          ([OGA]:re) -> case (head (tail s)) of
                                            (VLISTA vl2) -> (interpreter [] ((lazyE vl2 vl2):re) c1 ((TRIPLA (tail (tail s)) (tail e) (tail c)):d))
                                      Push ->(interpreter s  ([OGA]:e)  (tail c)  d)
                                      Stop -> (head s)


interpr x = interpreter [] [] (x++[Stop]) []
