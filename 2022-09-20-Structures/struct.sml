(* QN1 Quick sort algorithm*)

datatype order = LESS | GREATER | EQUAL
signature SORT = sig
    type t
    val sort : t list-> t list
end

signature ORD_KEY = 
sig
    type ord_key 
    val compare : ord_key * ord_key ->order
end


functor QSort (O : ORD_KEY): SORT = struct
    type t = O.ord_key
    fun boolCmp x y= O.compare(x,y)= GREATER
    fun sort [] = []
        | sort (x::xs)=let
                        val (L,R)=List.partition (boolCmp x) xs
                        in 
                        sort(L) @ [x] @ sort(R)
                        end
    
    
end

(*QN2 *)
structure IntOrd : ORD_KEY = struct
    type ord_key = int 
    val compare  = Int.compare
end



structure QSortInt = QSort(IntOrd)

(*to check *)
QSortInt.sort [2,5,4,1]
