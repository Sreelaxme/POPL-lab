(* QN 1 Tri variate function of curry and uncurry 

curry   : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)

uncurry : ('a -> 'b -> 'c -> d) -> ('a * 'b * 'c -> 'd)

*)

fun curry f a b c = f (a,b,c);

fun uncurry f (a,b,c) = f a b c;

(* Qn 2 fst: 'a * 'b -> 'a 
        snd: 'a * 'b -> 'b
 *)

fun fst (a,b)  = a;
fun snd (a,b)  = b;
  
(* Qn 3 length : 'a list-> int *)

fun length [x]     = 1
  | length []      = 0
  | length (x::xs) = length xs +1;

(* Qn 4 reverse : 'a list -> 'a list *)

fun helper  x []     = x
  | helper x (y::ys) = helper (y::x) ys;
fun reverse []       = []
  | reverse  (x::xs) = helper [] (x::xs);


(* Qn 5 fib : int -> int *)
fun fib 0  = 0
  | fib 1  = 1
  | fib x  = fib (x-1) + fib (x-2);


			  








		     
  

