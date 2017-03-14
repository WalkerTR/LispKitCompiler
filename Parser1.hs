module Parser1(
progdoll) where

import Lexer
import Prelude hiding (EQ,exp)

data Exc a = Raise Exception | Return a
type Exception = String

instance Show a => Show (Exc a) where
 show (Raise e)= "ERROR:" ++ e
 show (Return x) = "RESULT:" ++ (show x)

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
rec_key (a:b)                = Raise ("found " ++ show(a) ++", expected LET o LETREC")
rec_key  x                   = Raise ("ERRORE STRANO"  ++  show(x))

rec_in::[Token]->Exc[Token]
rec_in ((Keyword IN):b)= Return b
rec_in (a:b)           = Raise ("found " ++ show(a) ++ ", expected IN")

rec_end::[Token]->Exc[Token]
rec_end ((Keyword END):b)= Return b
rec_end (a:b)            = Raise ("found " ++ show(a) ++ ", expected END")


rec_then ((Keyword THEN):b)= Return b
rec_then (a:b)             = Raise ("found " ++ show(a) ++ ", expected THEN")


rec_else ((Keyword ELSE):b)= Return b
rec_else (a:b)             = Raise ("found " ++ show(a) ++ ", expected ELSE")


rec_lp ((Symbol LPAREN):b)=Return b
rec_lp (a:b)              = Raise ("found " ++ show(a) ++ ", expected ELSE")


rec_rp ((Symbol RPAREN):b)=Return b
rec_rp (a:b)              = Raise ("found " ++ show(a) ++ ", expected )")



rec_virg ((Symbol COMMA):b)=Return  b
rec_virg (a:b)               = Raise ("found " ++ show(a) ++ ", expected ,")



rec_equals ((Symbol EQUALS):b)= Return b
rec_equals (a:b)              = Raise ("found " ++ show(a) ++ ", expected =")



progdoll::[Token] -> String
progdoll x= show (prog x)

prog:: [Token] -> Exc [Token]
prog a = do
         x<-rec_key a
         y<-bind x
         z<-rec_in y
         w<-exp z
         rec_end w


exp::[Token]->Exc[Token]
exp a@((Keyword LET):b)    = (prog a)
exp a@((Keyword LETREC):b) = (prog a)
exp ((Keyword LAMBDA):b)   = do
                                x<-seq_var b
                                exp x
exp ((Operator CONS):b)    = do
                                x<-rec_lp b
                                y<-exp x
                                z<-rec_virg y
                                w<-exp z
                                rec_rp w
exp ((Operator LEQ):b)     = do
                                x<-rec_lp b
                                y<-exp x
                                z<-rec_virg y
                                w<- exp z
                                rec_rp w
exp ((Operator EQ):b)      = do
                                x<-rec_lp b
                                y<-exp x
                                z<- rec_virg y
                                w<-exp z
                                rec_rp w
exp ((Operator CAR):b)      = exp b
exp ((Operator CDR):b)      = exp b
exp ((Operator ATOM):b)     = exp b
exp ((Keyword IF):b)        = do
                                x<- exp b
                                y<-rec_then x
                                z<-exp y
                                w<-rec_else z
                                exp w
exp x                       =  expa x


bind ((Id a):b)            =  do
                                x<- rec_equals b
                                y<- exp x
                                funx y
bind (a:_)                  = Raise ("binder with "++ show(a) ++" on the left")

funx ((Keyword AND):b)     = bind b
funx a@((Keyword IN):b)    = Return a
funx (a:_)                 = Raise ("found "++show(a)++" after binder")



expa a = do
           x<- funt a
           fune1 x


funt a = do
           x<-funf a
           funt1 x



fune1 ((Symbol PLUS):b)    = do
                              x<- funt b
                              fune1 x
fune1 ((Symbol MINUS):b)   = do
                              x<-funt b
                              fune1 x
fune1 x                    = Return x


funt1 ((Symbol TIMES):b)   = do
                              x<-funf b
                              funt1 x
funt1 ((Symbol DIVISION):b)= do
                              x<-funf b
                              funt1 x
funt1 x                    = Return x


funf (a:b)                 = if (exp_const a) then Return b
                                              else fX (a:b)

fX ((Id _):b)              = fuy b
fX ((Symbol LPAREN):b)     = do
                              x<- expa b
                              rec_rp x
fX (a:_)                   = Raise ("ERROR in fX, found"++ show(a))



exp_const::Token ->Bool
exp_const (Number _)  =  True
exp_const Nil         =  True
exp_const (Bool _)    =  True
exp_const (String _)  =  True
exp_const  _          = False


fuy ((Symbol LPAREN):b)      =  do
                                 x<-seq_exp b
                                 rec_rp x
fuy x                        = Return x


seq_var :: [Token]-> Exc [Token]
seq_var a@((Id name):b) = seq_var b
seq_var a@((Symbol RPAREN):b) = return a
seq_var (a:b) = Raise (show(a))

seq_exp :: [Token]-> Exc [Token]
seq_exp a@((Keyword LET):b) = do
                            x <- exp a
                            pippo x
seq_exp a@((Keyword LETREC):b) = do
                            x <- exp a
                            pippo x
seq_exp a@((Id name):b) = do
                            x <- exp a
                            pippo x
seq_exp a@((Keyword LAMBDA):b) = do
                            x <- exp a
                            pippo x
seq_exp a@((Symbol LPAREN):b) = do
                            x <- exp a
                            pippo x
seq_exp a@((Keyword IF):b) = do
                            x <- exp a
                            pippo x
seq_exp a@((Operator _):b) = do
                            x <- exp a
                            pippo x
seq_exp a@(c:b)
  | exp_const c =            do
                            x <- exp a
                            pippo x
seq_exp (a:b) = Raise (show(a))



pippo :: [Token]-> Exc [Token]
pippo a@((Symbol COMMA):b) = seq_exp b
pippo a@((Symbol RPAREN):b) = return a
