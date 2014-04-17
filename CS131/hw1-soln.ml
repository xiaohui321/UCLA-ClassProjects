let rec clone ((e, n) : 'a * int) : 'a list =
  match n with
      0 -> []
    | _ -> e::(clone(e, n-1))

	
let rec rev l =
    match l with
    	  [] -> l
	| x::xs -> (rev xs) @ [x]

	    
let fastRev (l : 'a list) : 'a list =
  let rec revHelper(remain, sofar) =
    match remain with
	[] -> sofar
      | x::xs -> revHelper(xs, x::sofar)
  in
    revHelper(l, [])

      
let rec tails (l : 'a list) : 'a list list =
  match l with
      [] -> [[]]
    | _::xs -> l::(tails xs)


let rec penultimate (l: 'a list) : 'a option =
    match l with
	[] -> None
      | [x;y] -> Some(x)
      | x::xs -> penultimate xs 


let rec flatten (l: 'a list list) : 'a list =
    match l with
    	  [] -> []
	| x::xs -> x @ (flatten xs)

	    
let rec intOfDigits l =
    let rec helper l n =
      match l with
       [] -> n
     | x::xs -> helper xs (n*10 + x)
in helper l 0;;


let rec merge ((l1,l2) : int list * int list) : int list =
    match (l1, l2) with
      ([], _) -> l2
    | (_, []) -> l1
    | (x::xs, y::ys) when x<y -> x::(merge(xs, l2))
    | (_, y::ys) -> y::(merge(l1, ys))


let rec pack (l: 'a list) : 'a list list =
  match l with
      [] -> []
    | [_] -> [l]
    | x::xs ->
	let (y::ys)::rest = pack xs in
	  if x=y then (x::y::ys)::rest
	  else [x]::(y::ys)::rest
	    
	
let rec sort (l: int list) : int list =
    let rec insert x l =
      match l with
        [] -> [x]
      | y::ys -> if x < y then x::l else y::(insert x ys)
    in 
    match l with
      [] -> []
    | x::xs -> insert x (sort xs)
