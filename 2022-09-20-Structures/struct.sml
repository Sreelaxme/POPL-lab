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
    type t 
    fun sort xs = 
end


structure IntOrd = struct
    type t = int 
    fun compare (x,y) = Int.compare(x,y)
end

IntOrd.compare(3,5)
structure QSortInt = QSort(IntOrd)
open List