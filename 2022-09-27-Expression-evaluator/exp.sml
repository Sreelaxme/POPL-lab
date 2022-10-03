(*QN 1*)

datatype Expr = Const of real
                |Var of Atom.atom
                |Plus of Expr * Expr
                |Mul of Expr* Expr

datatype Stmt = Assignment of Atom.atom * Expr
                | Print of Expr

type Program = Stmt list
(*open AtomRedBlackMap*)

(*QN2*)
type Env = real AtomMap.map		 
(*eval:Env -> Expr -> real option*)
   (* Evaluates an expression. The result is real option to take care of
	   cases when there are undefined variables *)
fun eval (Env) (Var (x))          = AtomMap.find(Env,x)
    |eval (Env) (Const(a))        = (SOME (a))
    |eval (Env) (Plus (x,y))      =let
        val a = eval Env x 
        val b = eval Env y
        fun add (SOME(u)) (SOME(v)) = SOME(u+v)
    | add _ _ = NONE
    in 
        add a b 
    end
    |eval (Env) (Mul (x,y))       = let
        val a = eval Env x 
        val b = eval Env y
        fun mul (SOME (u)) (SOME(v)) = SOME(u*v)
        | mul _ _ = NONE 
        in 
            mul a b 
        end


(*Executes: Env -> Stmt -> Env*)
 (* Executes a single statement and returns the modified environment *)
fun execute Env (Assignment (u,e)) = let
    val a = eval Env e
    fun helper (SOME(value)) Env = AtomMap.insert(Env,u,value)
    | helper  _ Env  = Env
    in
        helper a Env  
    end
    |execute (Env) (Print(e)) = let
      val a = eval Env e
      fun printhelp (Env) (SOME (exp)) = print(Real.toString(exp))
      | printhelp (Env) NONE           = print(" ")
    in
      printhelp Env a
    end
    
val interpret : Program -> unit
    (* Run the program starting with an empty environment
	This is essentially a fold from the left.
	*)
