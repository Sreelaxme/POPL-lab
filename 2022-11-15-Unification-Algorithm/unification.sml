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
fun help (x::xs) (y::ys) = ((x,y)::(zip xs ys))

functor Unify (
            structure S : SIGNATURE
            structure V : VAR
) = struct
    datatype term = VAR of V.var
                |Apply of S.symbol * term list
    structure VarMap = RedBlackMapFn(V)
    type telsecope = term VarMap.map
    type equation = term * term

    
    fun unify (tel : telescope)(eqt: equation) : telescope = case eqt of 
                    (VAR x ,t) => unifyVar x t
                    |(s, VAR y)=> unifyVar y s
                    |(Apply(f,fargs), Apply(g,gargs))=> unifyList tel (zip farfs, gargs)
    and unifyVar (x: V.var) (t:term): telescope =AtomMap.singleton(x,t)
        |unifyVar (s: term) (y:V.var): telsecope=AtomMap.singleton(y,s) 
    and unifyList (tel : telescope) [] = SOME tel
        | unifyList (tel: telescope) ((h,t)::eqns) : telescope = let s = unify tel (h,t)
                                                                    in
                                                                    unifylist s eqns
                                                                    end
    end