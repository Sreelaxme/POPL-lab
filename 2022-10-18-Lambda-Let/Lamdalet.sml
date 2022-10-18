(*
QN1 
*)

open Atom

datatype expr       = VAR of Atom.atom
                    |Expression of expr*expr
                    |L of Atom.atom * expr
datatype lamdalet = VAR_let of Atom.atom
                    |Expression_let of lamdalet*lamdalet
                    |L_let of Atom.atom * lamdalet                
                    |Let of Atom.atom * lamdalet * lamdalet
datatype lamdaletrec = VAR_letrec of Atom.atom
                    |Expression_letrec of lamdaletrec*lamdaletrec
                    |L_letrec of Atom.atom * lamdaletrec               
                    |Letrec of Atom.atom * lamdaletrec * lamdaletrec

(*QN 2 
lamda let to lambda calculus
let x = e1 in e2  (lamda x .e2) e1
*)
fun letTolam (VAR_let(x)) = VAR(x)
    |letTolam (Expression_let(e1,e2)) = Expression (letTolam(e1), letTolam(e2) )
    |letTolam (L_let(x,e)) = L(x, letTolam(e))
    |letTolam (Let(x,e1,e2)) = Expression(L (x , letTolam(e2)),letTolam e1)