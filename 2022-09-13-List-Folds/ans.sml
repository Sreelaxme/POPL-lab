(*1

foldr : ( 'elem * 'summar -> 'summary ) -> 'summary -> 'elem list -> 'summary
foldl : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary

*)

fun foldr f s [] = s
  | foldr f s (x::xs) = f(x, foldr f s xs)

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
fun partHelp f (a,(xs, ys)) = if (f a ) then (a::xs,ys) else (xs,a::ys)
fun partition f xs = foldr (partHelp f) ([],[]) xs
fun f x = if x>2 then true else false
val x = partition f [1,3,4,0,5]

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

fun reverse x = let fun rhelp (x,y) = x::y
		in
		    foldl rhelp [] x
		 
		end

(*

nth : 'a list * int -> 'a option
nthAux : 'a list * int -> 'a find
*)		   

datatype 'a Find = LookingFor of int
		 | Found of 'a

fun find (x,(LookingFor 1)) = Found x
	|find (x ,(LookingFor n))  = LookingFor (n-1)
	|find (x, (Found v))  = Found v				
fun nthAux list y = foldl find (LookingFor y) list 

fun nhelp (Found x ) =SOME x
	| nhelp (LookingFor x) = NONE
fun nth list y = nhelp (nthAux list y)

val ntest = nth [1,2,3,4,5] 7