

(* Problem 1a
   map2: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
*)

let rec map2 f l1 l2 =
  match (l1,l2) with
      ([],[]) -> []
    | (x::xs,y::ys) -> (f x y)::(map2 f xs ys)

	
(* Problem 1b
   rev: 'a list -> 'a list
*)

let rev l = List.fold_right (fun x rest -> rest@[x]) l []

  
(* Problem 1c
   rev2: 'a list -> 'a list
*)

let rev2 l = List.fold_left (fun rest x -> x::rest) [] l

  
(* Problem 1d
   curry: ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
   uncurry: ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
*)

let curry f = (fun x y -> f(x,y))

let uncurry f = (function (x,y) -> f x y)

  
(* Problem 1e
   mapAllPairs: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
*)

let mapAllPairs f l1 l2 =
  let allPairsOne x l = List.map (function y -> (x,y)) l in
  let allPairs = List.fold_right (fun x rest -> (allPairsOne x l2)@rest) l1 [] in
    List.map (function (x,y) -> f x y) allPairs

      
(* Dictionaries *)    

(* Problem 2a
   empty1: unit -> ('a * 'b) list
   put1: 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
   get1: 'a -> ('a * 'b) list -> 'b
*)  

let empty1() = []
      
let put1 k v d = (k,v)::d

let rec get1 k d =
  match d with
      [] -> raise Not_found
    | (k',v')::d' -> if k=k' then v' else get1 k d'

	
(* Problem 2b
   empty2: unit -> ('a,'b) dict2
   put2: 'a -> 'b -> ('a,'b) dict2 -> ('a,'b) dict2
   get2: 'a -> ('a,'b) dict2 -> 'b
*)  
    
type ('a,'b) dict2 = Empty | Entry of 'a * 'b * ('a,'b) dict2

let empty2() = Empty
  
let put2 k v d = Entry (k,v,d)

let rec get2 k d =
  match d with
      Empty -> raise Not_found
    | Entry (k', v', d') ->
      if k'=k then v' else (get2 k d')

	
(* Problem 2c
   empty3: unit -> ('a,'b) dict3
   put3: 'a -> 'b -> ('a,'b) dict3 -> ('a,'b) dict3
   get3: 'a -> ('a,'b) dict3 -> 'b
*)  

type ('a,'b) dict3 = ('a -> 'b)

let empty3() = (function s -> raise Not_found)    
    
let put3 k v d =
  (fun k' -> if k=k' then v else (d k'))

let get3 k d = d k

  
(* A type for arithmetic expressions *)
  
type op = Plus | Minus | Times | Divide
type aexp = Num of float | BinOp of aexp * op * aexp

(* Problem 3a
   evalAExp: aexp -> float
*)

let evalOp (o:op) : float->float->float =
  match o with
      Plus -> (+.)
    | Minus -> (-.)
    | Times -> ( *. )
    | Divide -> ( /. )
  
let rec evalAExp (e:aexp) : float =
  match e with
      Num n -> n
    | BinOp (e1, o, e2) ->
	(evalOp o) (evalAExp e1) (evalAExp e2)


(* A type for stack operations *)	  
	  
type sopn = Push of float | Swap | Calculate of op

(* Problem 3b
   evalRPN: sopn list -> float
*)

(* evaluates a single stack operation and returns the updated stack *)
let evalRPNOne (s : float list) (o : sopn) : float list = 
  match (o, s) with
      (Push n, s) -> n::s
    | (Swap, n2::n1::ns) -> n1::n2::ns
    | (Calculate op, n2::n1::ns) -> ((evalOp op) n1 n2)::ns

let evalRPN l =
  let n::ns = List.fold_left evalRPNOne [] l
  in n

(* Problem 3c
   toRPN: aexp -> sopn list
*)

let rec toRPN (e:aexp) : sopn list =
  match e with
      Num n -> [Push n]
    | BinOp (e1, o, e2) ->
	(toRPN e1)@(toRPN e2)@[Calculate o]

(* Problem 3d
   toRPNopt: aexp -> (sopn list * int)
*)

let rec toRPNopt (e:aexp) : (sopn list * int) =
  match e with
      Num n -> ([Push n], 1)
    | BinOp (e1, o, e2) ->
	let (s1, n1) = toRPNopt e1 in
	let (s2, n2) = toRPNopt e2 in
	  if n1 >= n2 then
	    (s1@s2@[Calculate o], max n1 (1+n2))
	  else
	    let rest = 
	      match o with
		  Minus|Divide -> [Swap;Calculate o]
		| _ -> [Calculate o] in
	      (s2@s1@rest, n2)
