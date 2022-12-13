(*structure varMap = RedBlackMapFn*)

signature SIGNATURE = sig
    type symbol
    val arity : symbol -> int 
    val compare : symbol * symbol -> order
    val symbol : string -> symbol
    val toString : symbol -> string
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
    
    fun symbol "bool" = BOOL
        | symbol "int" = INT
        | symbol "->" = ARROW
    
    fun toString BOOL = "bool"
        | toString INT = "int"
        | toString ARROW = "->"

    fun compare (s1,s2) = Int.compare(toINT s1, toINT s2)
end
signature unification = sig
    type symbol
    type term
    type telescope
    type eqn

    val unify : telescope -> eqn -> telescope
    val unifyList : telescope ->eqn list -> telescope
    (* val var : Atom.atom -> term
    val app : symbol * term list -> term
    val toString : term -> string *)
end
(* signature VAR = sig
    type var 
    val fresh : unit -> var
    val toString : var -> string
    val compare : var * var -> order
end *)
fun help (x::xs) (y::ys) = ((x,y)::(help xs ys))

functor Unify (S : SIGNATURE):unification = struct
    type symbol = S.symbol
    datatype term = VAR of Atom.atom
                |Apply of symbol * term list
    
    (* structure VarMap = RedBlackMapFn(V) *)
    type telescope = term AtomMap.map
    type eqn = term * term

    fun unify (tel : telescope) (VAR x , t) = AtomMap.singleton(x,t)
    
        |unify (tel:telescope) (t , VAR x) = unify tel (VAR x , t)
        | unify (tel:telescope) (Apply (a,args1),Apply (b ,args2)) = unifyList tel (help args1 args2)
    (* and unifyVar (x: V.var) (t:term): telescope =AtomMap.singleton(x,t)
        |unifyVar (s: term) (y:V.var): telsecope=AtomMap.singleton(y,s)  *)
        
        and unifyList (tel : telescope) [] =  tel
        
        |unifyList (tel: telescope) ((h,t)::eqns) = unifyList (unify tel (h,t)) eqns
    end