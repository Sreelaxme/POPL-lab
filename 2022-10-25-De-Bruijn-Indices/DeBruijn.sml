(*QN 1*)
datatype deBruijn = deVar of int                (*VAR*)
                | dL of deBruijn                (*ABS*)
                | dExp of deBruijn * deBruijn   (*APP*)

datatype expr = VAR of string           (*VAR*)
                |Exp of expr * expr     (*APP*)
                |L of string * expr     (*ABS*)


(*Qn 2
    helper: char list -> char list -> char list
    diag : string -> string -> string
    fresh : string list -> string
*)


fun foldl f s []        = s
  | foldl  f s (x::xs) = foldl f (f(x,s)) xs


fun uncurry f(a,b)= f a b
fun helper [] [] = [#"a"]
    |helper [] (y::ys) = if y = #"a" then [#"b"]
                        else [#"a"]
    |helper s []        = s 
    |helper (x::xs) (y::ys) = if x=y then (x:: (helper xs ys))
                            else x::xs

fun diag s1 s2 = implode(helper (String.explode(s2)) (String.explode(s1)))


fun fresh (sets:string list) = foldl (uncurry(diag)) "" sets
