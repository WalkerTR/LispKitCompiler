import Lexer
import Parser2
import Compiler
import Interpreter

exec prog = interpr (comp_one (progdoll (lexi prog)))



factlist12345 =   "letrec \
                    \FACT = lambda ( X ) if eq ( X, 0 ) then 1 else  X*FACT(  X- 1 ) \
                    \and \
                    \G = lambda ( H L ) if  eq ( L,  nil ) then L else cons( H(car( L ) ), G ( H, cdr ( L ) )) \
                \in \
                    \G ( FACT, cons(1, cons(2, cons(3, cons(4, cons(5, nil))))) ) \
                \end$";

fact10 = "letrec \
            \FACT = lambda ( X ) if eq ( X, 0 ) then 1 else  X*FACT(  X- 1 ) \
        \in \
            \FACT ( 10 ) \
        \end$";

sum01 = "let x=0 and y=1 in let b=x+y in b end end $";
