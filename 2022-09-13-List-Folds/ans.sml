(*1

foldr : ( 'elem * 'summar -> 'summary ) -> 'summary -> 'elem list -> 'summary
foldl : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary

*)

fun foldr f s [] = s
  | foldr f s (x::
	       xs) = f(x, foldr f s xs)

fun foldl f s []        = s
  | foldl  f s (x::xs) = foldl f (f(x,s)) xs

(*2
sum : int list -> int
 *)
fun add (x,y)   = x + y
fun sum x  = foldr add 0 x	      



(*

partition: ('a -> bool ) -> 'a list -> 'a list * 'a list

*)


(*

map : ('a -> 'b ) -> 'a list -> 'b list	

*)
fun map f xs = let fun mhelp (x,y) = (f x)::y
	       in
		   foldl mhelp [] xs
		end

		   
(*

reverse : 'a list -> 'a list

*)
		   (*
fun reverse_helper x []   = x
  | reverse_helper x (y::ys) = reverse_helper (y::x) ys
fun reverse x = foldr reverse_helper [] x*)					      

fun reverse x = let fun rhelp (x,y) = y@[x]
		in
		    foldr rhelp [] x
		 
		end

(*

nth : 'a list * int -> 'a option

*)		   

datatype 'a find = LookingFor of int
	| Found of 'a
