
(* Name: Xiaohui, Zhou

   UID: 104-014-248

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*)

(* Problem 1a
   map2: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
*)
exception Invalid_argument of string

let rec map2 f l1 l2 =
    match l1 with
         [] -> []
      |  h1::t1 -> (match l2 with 
		      [] -> raise (Invalid_argument 
				     ("These two lists do not have a same length"))
		    |  h2::t2 -> (f h1 h2)::(map2 f t1 t2));;     	


(* Problem 1b
   rev: 'a list -> 'a list
*)
let rev a = 
  List.fold_right (fun x y -> y@[x]) a [];;


(* Problem 1c
   rev2: 'a list -> 'a list
*)
let rev2 a =
  List.fold_left (fun x y -> [y]@x) [] a;;


(* Problem 1d
   curry: ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
   uncurry: ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
*)
let curry f x y = (f (x,y));;

let uncurry f (x,y)  = (f x y);;   


(* Problem 1e
   mapAllPairs: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
*)
let mapAllPairs f l1 l2 = 
  List.concat (List.map (fun x -> (List.map (fun y -> f x y) l2)) l1);;


(* Dictionaries *)    

(* Problem 2a
   empty1: unit -> ('a * 'b) list
   put1: 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
   get1: 'a -> ('a * 'b) list -> 'b
*)  

let empty1 () : ('a * 'b) list = [];;

let put1 x y l = (x,y)::l;;
  
let rec get1 x l  = 
  match l with
      []  -> raise (Not_found)
   | (a,b)::t -> if a = x 
		 then b
		 else get1 x t;;
	
(* Problem 2b
   empty2: unit -> ('a,'b) dict2
   put2: 'a -> 'b -> ('a,'b) dict2 -> ('a,'b) dict2
   get2: 'a -> ('a,'b) dict2 -> 'b
*)  
    
type ('a,'b) dict2 = Empty | Entry of 'a * 'b * ('a,'b) dict2

let empty2 () = Empty;;

let put2 x y l =  Entry(x,y,l);;
			
let rec get2 x l =
  match l with
	  Empty -> raise (Not_found)
	| Entry (a,b,c) -> if a = x 
			      then b 
			      else get2 x c;;
	
(* Problem 2c
   empty3: unit -> ('a,'b) dict3
   put3: 'a -> 'b -> ('a,'b) dict3 -> ('a,'b) dict3
   get3: 'a -> ('a,'b) dict3 -> 'b
*)  

type ('a,'b) dict3 = ('a -> 'b)

let empty3 () : ('a , 'b) dict3 = (fun b -> raise Not_found);;

let put3 x y (f:('a, 'b) dict3 ) : ('a, 'b ) dict3  = (fun a -> if a = x then y else (f a));;
  
let get3 x (f:('a, 'b) dict3 ) : 'b = f x;;
  

(* Calculators *)    
  
(* A type for arithmetic expressions *)
  
type op = Plus | Minus | Times | Divide
type aexp = Num of float | BinOp of aexp * op * aexp

(* Problem 3a
   evalAExp: aexp -> float
*)
let rec evalAExp exp = 
  match exp with
	  Num x -> x
	| BinOp (a,b,c) -> match b with
				   Plus -> (evalAExp a) +. (evalAExp c)
				 | Minus -> (evalAExp a) -. (evalAExp c)
				 | Times -> (evalAExp a) *. (evalAExp c)
				 | Divide -> (evalAExp a) /. (evalAExp c);;

(* A type for stack operations *)	  
	  
type sopn = Push of float | Swap | Calculate of op

(* Problem 3b
   evalRPN: sopn list -> float
*)
exception Stack_error

let evalRPN_calculate op x y =
  match op with
    Plus -> x +. y
  | Minus -> x -. y
  | Times -> x *. y
  | Divide -> x /. y;;

let rec evalRPN_1 list stack =
  match list with
    [] -> (match stack with
	     [h] -> h
	   | _   -> raise Stack_error)
  
  | h::t ->(match h with
	      Push x -> evalRPN_1 t (x::stack) 
	    | Swap   ->  (match stack with
			    h1::h2::t1 -> evalRPN_1 t (h2::h1::t1)
			   | _ ->raise Stack_error)
	    | Calculate op -> (match stack with
				 h1::h2::t1 -> evalRPN_1 t ((evalRPN_calculate op h2 h1)::t1)
			       | _ ->raise Stack_error));;
				

let evalRPN list = evalRPN_1 list [];;

  
(* Problem 3c
   toRPN: aexp -> sopn list
*)
let rec toRPN exp = 
  match exp with
     Num x -> [Push x;]
   | BinOp (exp1, op, exp2) -> (toRPN exp1)@(toRPN exp2)@[Calculate op];;
 
  
(* Problem 3d
   toRPNopt: aexp -> (sopn list * int)
*)


let rec toRPNopt1 exp = 
  match exp with
    Num x -> ([Push x;] , 1)
  |  BinOp (exp1, op, exp2) -> let (s1,n1) = toRPNopt1 exp1 in
			       let (s2,n2) = toRPNopt1 exp2 in
			       if n1 >= n2 
			       then
				 (s1@s2@[Calculate op], (n1+n2+1))
			       else
				 (s2@s1@[Swap]@[Calculate op], (n1+n2+1));;

let toRPNopt exp = let (x,y) = toRPNopt1 exp in x;;
