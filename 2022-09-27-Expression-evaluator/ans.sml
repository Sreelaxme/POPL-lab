(**)
datatype = option = SOME of a | NONE

datatype Expr = Const of real
             | VAR of a
             | PLUS of Expr * Expr
             | MUL of Expr * Expr
