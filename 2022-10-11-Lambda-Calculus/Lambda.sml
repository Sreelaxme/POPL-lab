(*Qn 1*)


open Atom

datatype expr = VAR of Atom.atom
                |Expression of expr*expr
                |L of Atom.atom * expr

(*
Qn2
free  : expr -> atom set
bound : expr -> atom set

*)


fun free (VAR (x))                   = AtomSet.singleton(x)
    |free (Expression( exp1,exp2) )  = AtomSet.union(free(exp1),free(exp2))
    |free (L (x,e ))                 = AtomSet.subtract(free(e),x)
fun bound (VAR (x))                   = AtomSet.empty
    |bound (Expression( exp1,exp2) )  = AtomSet.union(bound(exp1),bound(exp2))
    |bound(L (x,e ))                 = AtomSet.add(bound(e),x)

(* Qn3 
subst : expr -> atom -> expr -> expr

*)
fun subst (VAR(y)) x M:expr = if Atom.same(y,x) then M 
                        else VAR(y)
    |subst (Expression(e1,e2)) x M:expr = Expression (subst e1 x M ,subst e2 x M)
    |subst (L(y , e)) x M:expr = if Atom.same(y,x) then L(x,e)
                            else L(y , subst e x M)
   
