(*************************
 * Xiaohui, Zhou
 * 104-014-248
 *) 
(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
*)
exception DynamicTypeError 

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
 *)
let rec patMatch (pat:mopat) (value:movalue) : moenv = 
  
  match (pat, value) with
    (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
    (IntPat(i), IntVal(j))  when i=j -> Env.empty_env()
  | (BoolPat(i),BoolVal(j)) when i=j -> Env.empty_env()
  | (WildcardPat, _ )                -> Env.empty_env()
  | (NilPat, ListVal(NilVal))        -> Env.empty_env()
  | (VarPat(i), anyVal)              -> Env.add_binding i anyVal (Env.empty_env())
  | (ConsPat(i,j),ListVal(k))        -> 
                            (match k with
			       NilVal           -> raise MatchFailure
			     | ConsVal(mov,mol) -> Env.combine_envs (patMatch i mov) 
								    (patMatch j (ListVal(mol))))
  | _  -> raise MatchFailure;;


(*******************************************************************************)
    
(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.

*)
let evalBinOpExpr (v1 : movalue) (v2 : movalue) (op : moop) : movalue =
  match op with
    Plus   ->  (match (v1,v2) with
		  (IntVal(i),IntVal(j))    -> IntVal(i + j) 
		| _                        -> raise (DynamicTypeError ))		 
  | Minus  ->  (match (v1,v2) with
		  (IntVal(i),IntVal(j))    -> IntVal(i - j) 
		| _                        -> raise (DynamicTypeError ))		 
  | Times  ->  (match (v1,v2) with
		  (IntVal(i),IntVal(j))    -> IntVal(i * j) 
		| _                        -> raise (DynamicTypeError ))		 
  | Eq     ->  (match (v1,v2) with
		  (IntVal(i),IntVal(j))    -> BoolVal(i = j) 
		| (BoolVal(i),BoolVal(j))  -> BoolVal(i = j) 
		| (ListVal(i),ListVal(j))  -> BoolVal(i = j)
		| _                        -> raise (DynamicTypeError ))		 
  | Gt     ->  (match (v1,v2) with
		  (IntVal(i),IntVal(j))    -> BoolVal(i > j) 
		| _                        -> raise (DynamicTypeError ))		 
  | Cons   ->  (match (v1, v2) with
		  (anyVal, ListVal(j))     -> ListVal(ConsVal(anyVal,j))
		| _                        -> raise (DynamicTypeError ));;
     
let rec evalMatchExpr (v: movalue) (list: (mopat * moexpr) list) : (moexpr * moenv) =
  match list with
    []   -> raise DynamicTypeError
  | (mpat,mexpr)::t -> try (mexpr, (patMatch mpat v)) with MatchFailure -> evalMatchExpr v t;;

let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
      IntConst(i)               -> IntVal(i)
    | BoolConst(i)              -> BoolVal(i)
    | Nil                       -> ListVal(NilVal)
    | Var(s)                    -> (try Env.lookup s env with Env.NotBound -> 
							   raise (DynamicTypeError))
    | If(mexp1,mexp2,mexp3)     -> 
                                  (match evalExpr mexp1 env with
				     BoolVal(b) -> 
				                  if b = true then evalExpr mexp2 env
						              else evalExpr mexp3 env
				   | _  -> raise (DynamicTypeError))
    | BinOp(mexp1,mop,mexp2)    -> (let mov1 = evalExpr mexp1 env in
				    let mov2 = evalExpr mexp2 env in
				    evalBinOpExpr mov1 mov2 mop)
    | Match(mexp,list)          -> (let mov = evalExpr mexp env in
				    let (exp,env2) = evalMatchExpr mov list in
				   evalExpr exp (Env.combine_envs env env2))
    | Function(mpat,mexp)       -> FunctionVal(None,mpat,mexp,Env.empty_env())
    | FunctionCall(mexp1,mexp2) -> let fmov = evalExpr mexp2 env in
				   let fv = evalExpr mexp1 env  in
				   match fv with
				     FunctionVal(fname,fpat,fexp,fenv) -> 
				       let  newfenv = (match fname with
							 None       -> fenv
						       | Some(name) -> Env.add_binding name 
										       fv fenv) in
				       let envIn = patMatch fpat fmov in
				       evalExpr fexp (Env.combine_envs envIn newfenv)
				   | _ -> raise (DynamicTypeError)


(*
    | FunctionCall(mexp1,mexp2) -> (match evalExpr mexp1 env with
				      FunctionVal(fname,fpat,fexp,fenv) -> 
				         let fmov = evalExpr mexp2 env in
					 let env2 = patMatch fpat fmov in
					 evalExpr fexp (Env.combine_envs env2 fenv)
				    | _ -> raise (DynamicTypeErro "i"))
 *)


(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's top-level expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      Expr(e)               -> (None, evalExpr e env)
    | Let(str,mexp)         -> (Some(str),(evalExpr mexp env))
    | LetRec(str,mpat,mexp) -> let mval = FunctionVal(Some(str),mpat,mexp,env) in
			       (Some(str),mval)  


