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
fun eval (env:Env) (Var (x))          = AtomMap.find(env,x)
    |eval (env:Env) (Const(a))        = (SOME (a))
    |eval (env:Env) (Plus (x,y))      =let
               
        fun add (SOME(u)) (SOME(v)) = SOME(u+v)
    | add _ _ = NONE
    in 
        add (eval env x) (eval env y)
    end
    |eval (env:Env) (Mul (x,y))       = let
       
        fun mul (SOME (u)) (SOME(v)) = SOME(u*v)
        | mul _ _ = NONE 
        in 
            mul (eval env x) (eval env y)
        end


(*Executes: Env -> Stmt -> Env*)
 (* Executes a single statement and returns the modified environment *)
fun execute (env:Env) (Assignment (u,e)) = let
    
    fun helper (SOME(value)) env = AtomMap.insert(env,u,value)
    | helper  _ env  = env
    in
        helper (eval (env:Env) e) env:Env  
    end
    |execute (env:Env) (Print(e)) = let
	fun printhelp (SOME (exp)) = print(Real.toString(exp))
	  | printhelp NONE = print(" ") 
      
        val k = printhelp (eval env e)
    in
	    env
    end
(*
    
val interpret : Program -> unit
     Run the program starting with an empty environment
	This is essentially a fold from the left.
*)
				    
fun interpret prgrm = let
    val finalEnv = let
	fun exec (statement, env) = execute env statement
    in
	List.foldl exec AtomMap.empty prgrm
    end
in
    ()
end
			  
