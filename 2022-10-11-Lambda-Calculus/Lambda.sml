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
fun subst (VAR(y)) x M:expr             = if Atom.same(y,x) then M 
                                        else VAR(y)
    |subst (Expression(e1,e2)) x M:expr = Expression (subst e1 x M ,subst e2 x M)
    |subst (L(y , e)) x M:expr          = if Atom.same(y,x) then L(x,e)
                                        else L(y , subst e x M)
   

(*QN4
fresh : atom set -> atom
diag : string -> string -> string

*)
open String
fun foldl f s []        = s
  | foldl  f s (x::xs) = foldl f (f(x,s)) xs


fun uncurry f(a,b)= f a b
fun helper [] [] = [#"a"]
    |helper [] (y::ys) = if y = #"a" then [#"b"]
                        else [#"a"]
    |helper s []        = s 
    |helper (x::xs) (y::ys) = if x=y then (x:: (helper xs ys))
                            else x::xs

fun diag s1 s2 = implode(helper (String.explode(s1)) (String.explode(s2)))

fun diagA s1 s2 = implode(helper (explode(s1)) (explode(Atom.toString(s2))))
fun diagB s1 s2 = (diagA s2 s1)

fun fresh sets = Atom.atom(foldl (uncurry(diagB)) "" sets)

val t = Atom.toString(fresh [Atom.atom("a"), Atom.atom("b"),Atom.atom("ba")])