(*Qn1
  foldr : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary
  foldl : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary

 *)

fun foldr f s []          = s
   | foldr f s (x :: xs)  = f (x ,foldr f s xs) 

fun foldl f s []         = s
  | foldl f s (x::xs)    = foldl f (f(x,s)) xs

(*
fun nth ([], n) = none
  | nth (x::xs, n) = if n< 0 then NONE
		     else if n==0 then x
		     else nth(xs, n-1
			     )

fun partition *)

(* Qn2 
   sum : int list -> int 
 *)
fun add (x,y) = x+y
fun sum p = foldl add 0 p
			

(* Qn3 
 partition : ('a -> bool) -> 'a list -> 'a list * 'a list
 *)

fun map f xs = let fun mhelp (x,xs) = (f x)::xs
	       in
		   foldr mhelp [] xs
	       end
fun reverse =let fun rhelp (xs,x) = (reverse xs)@ x
	     in foldr rhelp [] xs
 	end
(*fun partition f xs =
    let
       p 
    in
	f (xs, ([],[]))
    end
*)
