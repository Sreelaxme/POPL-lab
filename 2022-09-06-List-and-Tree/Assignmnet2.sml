(* Qn 1: Map functions

   map : ('a->'b)->'a list ->'b list
 *)
(*
fun plusOne x = x+1 *)

		      
fun map f []      = []  
  | map f (x::xs) =(f x) ::( map f xs)



(*Qn 2:
   Datatype that captures a tree
 *)

			       
					      

datatype 'a tree = empty | node of 'a tree * 'a  * 'a tree

(*Qn 3 : 
   treemap:'a tree -> 'a tree	 *)
fun treemap f empty           = empty
  | treemap f (node (l,x,r))  = node ( (treemap f l) ,f x, (treemap f r) )  


(*Q4 

inorder : 'a tree -> 'a list
preorder: 'a tree -> 'a list
postorder: 'a tree -> 'a list
 *)
fun inorder empty           = []
  | inorder (node(l,x,r))   = inorder l @ x :: inorder r
fun preorder empty          = []
  | preorder (node(l,x,r))  = ( x ::(preorder l)) @ preorder r
fun postorder empty  	    = []
  | postorder (node(l,x,r)) = postorder l @ postorder r @ [x] 

(*Qn 5 : 
rotateClock : 'a tree->'a tree*)

fun  rotateClock (node(node(l1,x1,r1),x,r)) = node(l1,x1,node(r1,x,r))
   | rotateClock x = x 
