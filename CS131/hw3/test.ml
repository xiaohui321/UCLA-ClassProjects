
(* A simple test harness for the MOCaml interpreter. *)

(* put your tests here:
   each test is a pair of a MOCaml declaration and the expected
   result, both expressed as strings.
   use the string "dynamic type error" if a DynamicTypeError is expected to be raised.
   use the string "match failure" if a MatchFailure is expected to be raised.
   use the string "implement me" if an ImplementMe exception is expected to be raised

   call the function runtests() to run these tests
*)
let tests = [
  ("3", "3"); 
  ("false", "false");
  ("let x = 34", "val x = 34");
  ("y", "dynamic type error");
  ("x + 4", "38");
  ("let z = function y -> y +4 + nonexistingfunc var","val z = "); 
  (*make func with nonexisting variable and function *)
  ("z 2", "dynamic type error"); (*test our non working function*)
  ("let z = function y -> y +4 + x","val z = "); (*make one that works*)
  ("z 2","40"); (*use it*)(*why is it 40 and not 44? we didn't use "let x = x + 4" *)
  ("let count = function list -> match list with [] -> 0 | x::xs -> 1 + count xs","val count = ");
  ("let count = function list -> match list with [] -> 0 | x::xs -> 1 + count xs","val count = "); 
  (*should still not work as first count still has no count to reference*)
  ("let rec count list = match list with [] -> 0 | x::xs -> 1 + count xs","val count = ");
  (*make one that works*)
  ("let mylist = [4;5;true]","val mylist = [4; 5; true]"); (*make a list*)
  ("count mylist","3"); (*count the list*)
  
  (* Declarations *)
  ("3", "3"); 
  ("false", "false");
  ("let  i1 = 10", "val i1 = 10");
  ("let  i2 = 34", "val i2 = 34");
  ("let  b1 = false", "val b1 = false");
  ("let  b2 = true", "val b2 = true");
  ("let x = 34", "val x = 34");
  ("x" , "34");
  ("y", "dynamic type error");
  ("x + 4", "38");
  ("let b = true" ,"val b = true");
  ("x + b", "dynamic type error");
  
  (* BinOp *)
  ("let  i3 = i1 * i2", "val i3 = 340");
  ("let i3double = i3 + i3", "val i3double = 680");
  ("i3 > i1", "true");
  ("i1 = 10", "true");
  ("b1 = b2", "false"); 
  ("5 + 6", "11");
  ("13 - 3", "10");
  ("15 * 2", "30");
  ("5 = 1" , "false");
  ("5 = 5" , "true");
  ("5 > 1" , "true");
  ("5 > 5" , "false");
  ("5 > false" , "dynamic type error");
  ("i3 > false" , "dynamic type error");
  ("5 > b1" , "dynamic type error");
  ("true > false" , "dynamic type error");
  ("true = true" , "true");
  ("false = true" , "false");
  ("1 = true" , "dynamic type error");
  ("true = 10" , "dynamic type error");
  ("true + false" , "dynamic type error");
  ("true - false" , "dynamic type error");
  ("true * false" , "dynamic type error");
  ("5 + false", "dynamic type error");
  ("[5] > 7" , "dynamic type error");
  ("[5] = 5" , "dynamic type error");
  ("[17] * 40" , "dynamic type error");
  ("30 - [15]" , "dynamic type error");
  (* Basic list syntax *)
  ("let l1 = [1;2;3;4]" , "val l1 = [1; 2; 3; 4]");
  ("[]" , "[]");
  ("[5]" , "[5]");
  ("[5; true]" , "[5; true]");
  ("5::true::[]" , "[5; true]");
  ("1::2::3::4::[]" , "[1; 2; 3; 4]");
  ("[]::5" , "dynamic type error");
  ("[50]::10" , "dynamic type error");
  ("[]::false" , "dynamic type error");
  ("[50]::true" , "dynamic type error");
  ("true::1" , "dynamic type error");
  ("2::false" , "dynamic type error");
  ("true::true" , "dynamic type error");
  ("5::7" , "dynamic type error"); 
  ("5::garbage" , "dynamic type error");
  (* If statements *)
  ("if true then 5 else false" , "5");
  ("if false then 5 else false" , "false");
  ("if i1>5 then i1 else 5" , "10");
  ("if i3=i1 then b1 else b2" , "true");
  ("if (5+10)>(12 -1) then (if b2 then 100 else true) else false" , "100");
  ("if (5+10)=(12 -1) then (if b2 then 100 else true) else false" , "false");
  ("if 1 then 5 else 10" , "dynamic type error");
  ("if i1 then 15 else 10" , "dynamic type error");
  ("if true then true + false else 14 " , "dynamic type error");
  ("if 1::2::3::[] then 5 else 10" , "dynamic type error");
  ("if true = 1 then 10 else 1" , "dynamic type error"); 
  
 
  (* Pattern Matching *)
  ("match true  with  10 -> 100" , "dynamic type error");
  ("match  10 with 14  -> true " , "match failure");
  ("match (10+5) with 0 -> false | 15 -> true" , "true");
  ("match  (i3*2) with i2 -> (i2 + 1 )| i3double -> i1  " , "681");
  ("match  (function x->x) with  14 ->  10" , "dynamic type error");
  ("match [] with  10 -> 14 " , "dynamic type error");
  ("match  true with  [] ->  1" , "dynamic type error");
  ("match  [] with  true -> 0 " , "dynamic type error");
  ("match  10 with []  -> 1  " , "dynamic type error");
  ("match  10 with true  -> 0  " , "dynamic type error");
  ("match  l1 with a::b::c::d::ds -> a + b + c + d ", "10");
  ("match  l1 with  x::xs -> x*7  ", "7");
  ("match  [1;2] with a::b::c::d -> false | x::y::z -> z", "[]");
  ("match  [1;2] with x::xs -> x+1", "2");
  ("match  [] with x::xs -> false | [] -> true  ", "true");
  ("match  [] with x::xs -> false | l -> true  ", "true");
  ("match [1;2] with x::y::z::xs -> false | [] -> true | _ -> 10", "10");
  
  (* Non-Recursive Functions *)
  ("let f = (function x -> x + 1)" , "val f = <fun>");
  ("f 100" , "101");
  ("f true", "dynamic type error");
  ("f x", "35");
  ("let add = (function x ->  (function y -> x + y) )" ,"val add = <fun>");
  ("add 4 5" , "9");
  ("let head = (function x -> (match x with y::ys -> y | _ -> false))", "val head = <fun>");
  ("head [5;6;7]", "5");
  ("head []", "false");
  ("head 5" ,"dynamic type error");
  
  (* Recursive Functions *)
  ("let rec mapi l = match l with [] -> [] | x::xs -> (x+1)::(mapi xs)", "val mapi = <fun>");
  ("mapi [1;2;3;4]", "[2; 3; 4; 5]");
  ("let inc = function a -> a + 1", "val inc = <fun>");
  ("inc 5", "6");
  ("let rec map l = (function f -> match l with [] -> [] | x::xs -> (f x)::(map xs inc) )", "val map = <fun>");
  ("map [1;2;3;4] inc" , "[2; 3; 4; 5]");
  ("map [1;2;3;4] (function x -> x * 2)" , "[2; 4; 6; 8]");
]

(* The Test Harness
   You don't need to understand the code below.
*)	  

  
let testOne test env =
  let decl = main token (Lexing.from_string (test^";;")) in
  let res = evalDecl decl env in
  let str = print_result res in
  match res with
      (None,v) -> (str,env)
    | (Some x,v) -> (str, Env.add_binding x v env)
      
let test tests =
  let (results, finalEnv) =
    List.fold_left
      (fun (resultStrings, env) (test,expected) ->
	let (res,newenv) =
	  try testOne test env with
	      Parsing.Parse_error -> ("parse error",env)
	    | DynamicTypeError -> ("dynamic type error",env)
	    | MatchFailure -> ("match failure",env)
	    | ImplementMe s -> ("implement me",env) in
	(resultStrings@[res], newenv)
      )
      ([], Env.empty_env()) tests
  in
  List.iter2
    (fun (t,er) r ->
      let out = if er=r then "ok" else "expected " ^ er ^ " but got " ^ r in
      print_endline
	(t ^ "....................." ^ out))
      tests results

(* CALL THIS FUNCTION TO RUN THE TESTS *)
let runtests() = test tests
  
