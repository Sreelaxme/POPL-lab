(*structure varMap = RedBlackMapFn*)

signature SIGNATURE = sig
    type symbol
    val arity : symbol -> int 
    val compare : symbol * symbol -> order
end

structure TypeSig : SIGNATURE = struct
    datatype symbol = INT
                    | BOOL
                    | ARROW
    
    fun arity INT = 0
        | arity BOOL = 0
        | arity ARROW = 2

    fun toINT INT = 0
        |toINT BOOL = 1
        |toINT ARROW = 2

    fun compare (s1,s2) = Int.compare(toINT s1, toInt s2)
end

signature VAR = sig
    type var 
    val fresh : unit -> var
    val toString : var -> string
    val compare : var * var -> order
end

functor Unify (structure S : SIGNATURE
            structure V : VAR
) =
struct
    datatype term = Var of V.var
                |Apply of S.symbol * term list
    type telsecope = term.AtomMap.map
    type equation = term * term

    fun unify (tel : telescope)(eqt: equation) : telescope =
    case eqt of
    
end