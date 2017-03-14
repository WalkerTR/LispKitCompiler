module Parser2(
progdoll,LKC(..)) where

import Lexer
import Prelude hiding (EQ,exp)

data Exc a = Raise Exception | Return a
type Exception = String

data LKC =  ETY |
            VAR String | NUM Integer | STRI String | BOO Bool |
            NIL | ADD LKC LKC | SUB LKC LKC | MULT LKC LKC |
            REM LKC LKC | DIV LKC LKC | EQC LKC LKC | LEQC LKC LKC |
            CARC LKC | CDRC LKC | CONSC LKC LKC | ATOMC LKC |
            IFC LKC LKC LKC | LAMBDAC [LKC] LKC | CALL LKC [LKC] |
            LETC LKC [(LKC,LKC)] | LETRECC LKC [(LKC, LKC)]
            deriving(Show, Eq)

instance Show a => Show (Exc a) where
 show (Raise e)= "ERRORE:" ++ e
 show (Return x) = "RAGGIUNTO:" ++ (show x)

instance Functor Exc where
 fmap _ (Raise e) = Raise e
 fmap f (Return x) = Return (f x)

instance Applicative Exc where
 pure x  = Return x
 (<*>) (Raise e) _ = Raise e
 (<*>) (Return f) (Return x) = Return (f x)

instance Monad Exc where
 return x  = Return x
 (Raise e) >>= q   = Raise e
 (Return x) >>= q  = q x

raise :: Exception -> Exc a
raise e = Raise e

rec_key::[Token]-> Exc [Token]
rec_key ((Keyword LET):b)    = Return b
rec_key ((Keyword LETREC):b) = Return b
rec_key (a:b)                = Raise ("trovato " ++ show(a) ++", atteso LET o LETREC")
rec_key  x                   = Raise ("ERRORE STRANO"  ++  show(x))

rec_in::[Token]->Exc[Token]
rec_in ((Keyword IN):b)= Return b
rec_in (a:b)           = Raise ("trovato " ++ show(a) ++ ", atteso IN")

rec_end::[Token]->Exc[Token]
rec_end ((Keyword END):b)= Return b
rec_end (a:b)            = Raise ("trovato " ++ show(a) ++ ", atteso END")


rec_then ((Keyword THEN):b)= Return b
rec_then (a:b)             = Raise ("trovato " ++ show(a) ++ ", atteso THEN")


rec_else ((Keyword ELSE):b)= Return b
rec_else (a:b)             = Raise ("trovato " ++ show(a) ++ ", atteso ELSE")


rec_lp ((Symbol LPAREN):b)=Return b
rec_lp (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso ELSE")


rec_rp ((Symbol RPAREN):b)=Return b
rec_rp (a:b)              = Raise ("trovato " ++ show(a) ++ ", attesa )")



rec_virg ((Symbol COMMA):b)=Return  b
rec_virg (a:b)               = Raise ("trovato " ++ show(a) ++ ", attesa ,")



rec_equals ((Symbol EQUALS):b)= Return b
rec_equals (a:b)              = Raise ("trovato " ++ show(a) ++ ", atteso =")



progdoll::[Token] -> LKC
progdoll x = let
                res = prog x
              in
                case res of
                  Return (tkn,lkc) -> lkc
                  _ -> ETY

prog:: [Token] -> Exc ([Token],LKC)
prog a@((Keyword LET):b) = do
                     x<-rec_key a
                     (y,q)<-bind x
                     z<-rec_in y
                     (w,r)<-exp z
                     k<-rec_end w
                     return (k, LETC r q)
prog a@((Keyword LETREC):b) = do
                     x<-rec_key a
                     (y,q)<-bind x
                     z<-rec_in y
                     (w,r)<-exp z
                     k<-rec_end w
                     return (k, LETRECC r q)


exp::[Token]->Exc ([Token],LKC)
exp a@((Keyword LET):b)    = (prog a)
exp a@((Keyword LETREC):b) = (prog a)
exp ((Keyword LAMBDA):b)   = do
                                k<-rec_lp b
                                (x,y)<-seq_var k
                                t<-rec_rp x
                                (w,z)<-exp t
                                return (w, LAMBDAC y z)
exp ((Operator CONS):b)    = do
                                x<-rec_lp b
                                (y,q)<-exp x
                                z<-rec_virg y
                                (w,z)<-exp z
                                k<-rec_rp w
                                return (k, CONSC q z)
exp ((Operator LEQ):b)     = do
                                x<-rec_lp b
                                (y,q)<-exp x
                                z<-rec_virg y
                                (w,z)<-exp z
                                k<-rec_rp w
                                return (k, LEQC q z)
exp ((Operator EQ):b)      = do
                                x<-rec_lp b
                                (y,q)<-exp x
                                z<-rec_virg y
                                (w,z)<-exp z
                                k<-rec_rp w
                                return (k, EQC q z)
exp ((Operator CAR):b)      = do
                                (w,z)<-exp b
                                return (w, CARC z)
exp ((Operator CDR):b)      = do
                                (w,z)<-exp b
                                return (w, CDRC z)
exp ((Operator ATOM):b)     = do
                                (w,z)<-exp b
                                return (w, ATOMC z)
exp ((Keyword IF):b)        = do
                                (x,t)<-exp b
                                y<-rec_then x
                                (z,u)<-exp y
                                w<-rec_else z
                                (q,p)<-exp w
                                return (q, IFC t u p)
exp x                       =  expa x


bind :: [Token] -> Exc ([Token], [(LKC, LKC)])
bind ((Id a):b)            =  do
                                x<- rec_equals b
                                (y,t)<- exp x
                                (z, w)<-funx y
                                return (z, ((VAR a),t):w)

bind (a:_)                  = Raise ("BINDER CON "++ show(a) ++" A SINISTRA")

funx :: [Token] -> Exc ([Token], [(LKC, LKC)])
funx ((Keyword AND):b)     = bind b
funx a@((Keyword IN):b)    = Return (a, [])

funx (a:_)                 = Raise ("DOPO BINDERS; TROVATO"++show(a))


expa::[Token]->Exc ([Token],LKC)
expa a = do
           (x,y)<- funt a
           fune1 x y


funt::[Token]->Exc([Token],LKC)
funt a = do
           (x,y)<-funf a
           funt1 x y


fune1::[Token]->LKC->Exc([Token],LKC)
fune1 ((Symbol PLUS):b) l    = do
                              (x,y)<- funt b
                              fune1 x (ADD l y)
fune1 ((Symbol MINUS):b) l   = do
                              (x,y)<- funt b
                              fune1 x (SUB l y)
fune1 x y                    = return (x,y)


funt1::[Token]->LKC->Exc([Token],LKC)
funt1 ((Symbol TIMES):b) l  = do
                              (x,y)<-funf b
                              funt1 x (MULT l y)

funt1 ((Symbol DIVISION):b) l  = do
                              (x,y)<-funf b
                              funt1 x (DIV l y)
funt1 x y                    = return (x,y)


funf::[Token]->Exc([Token],LKC)
funf ((Number n):b) = return (b,(NUM n))
funf ((Nil):b) = return (b,NIL)
funf ((Bool a):b) = return (b,(BOO a))
funf ((String s):b) = return (b,(STRI s))
funf ((Id name):b) = do
                    (t,x)<-fuy b
                    case x of
                      [] -> return (t,VAR name)
                      l -> return (t,CALL (VAR name) l)
funf ((Symbol LPAREN):b)     = do
                              (x,y)<- expa b
                              z<-rec_rp x
                              return (z,y)
funf (a:_)                   = Raise ("ERRORE in funx, TROVATO"++ show(a))



fuy::[Token]->Exc([Token],[LKC])
fuy ((Symbol LPAREN):b)      =  do
                                 (x,y)<-seq_exp b
                                 z<-rec_rp x
                                 return (z,y)
fuy x                        = return (x,[])


seq_var :: [Token]-> Exc ([Token],[LKC])
seq_var a@((Id name):b) = do
                              (x,y)<-seq_var b
                              return (x,(VAR name):y)
seq_var a@((Symbol RPAREN):b) = return (a,[])
seq_var x = error (show(x))

seq_exp :: [Token]->Exc([Token],[LKC])
seq_exp a@((Keyword LET):b) = do
                            (x,y) <- exp a
                            (w,z) <- pippo x
                            return (w, y:z)
seq_exp a@((Keyword LETREC):b) = do
                            (x,y) <- exp a
                            (w,z) <- pippo x
                            return (w, y:z)
seq_exp a@((Id name):b) = do
                            (x,y) <- exp a
                            (w,z) <- pippo x
                            return (w, y:z)
seq_exp a@((Keyword LAMBDA):b) = do
                            (x,y) <- exp a
                            (w,z) <- pippo x
                            return (w, y:z)
seq_exp a@((Symbol LPAREN):b) = do
                            (x,y) <- exp a
                            (w,z) <- pippo x
                            return (w, y:z)
seq_exp a@((Keyword IF):b) = do
                            (x,y) <- exp a
                            (w,z) <- pippo x
                            return (w, y:z)
seq_exp a@((Operator _):b) = do
                            (x,y) <- exp a
                            (w,z) <- pippo x
                            return (w, y:z)
seq_exp a@((Number _):b) = do
                            (x,y) <- exp a
                            (w,z) <- pippo x
                            return (w, y:z)
seq_exp a@((Nil):b) = do
                            (x,y) <- exp a
                            (w,z) <- pippo x
                            return (w, y:z)
seq_exp a@((Bool _):b) = do
                            (x,y) <- exp a
                            (w,z) <- pippo x
                            return (w, y:z)
seq_exp a@((String _):b) = do
                            (x,y) <- exp a
                            (w,z) <- pippo x
                            return (w, y:z)
seq_exp (a:b) = Raise (show(a))



pippo :: [Token]-> Exc ([Token],[LKC])
pippo a@((Symbol COMMA):b) = seq_exp b
pippo a@((Symbol RPAREN):b) = return (a,[])
