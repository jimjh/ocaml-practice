(* binding - reminder about let and letrec *)

let foo x = x + 1 in
let bar x = foo (x - 1)
in bar 3110

let foo x = x + 1
and bar x = foo (x - 1) (* error - foo is unbound *)
in bar 3110

let rec foo x = bar (x - 1)
and bar x = x + 1
in foo 3110  (* functions foo and bar can refer to themselves and each other *)

let rec x = 1
and y = x + 1 (* error *)
in x + y

let square x = x *. x

(* form of function definitions we have been using, float * float -> float *)
let zig (x, y) = x +. 2. *. y

(* alternative form using pattern matching *)
let zig z = match z with (x, y) -> x +. 2. *. y

(* curried form float -> float -> float *)
let zigc x y = zig (x, y)

(* as an anonymous function *)
let zigc = fun x -> fun y -> zig (x, y)

let z = zigc (square 2.) 1.

(* partial evaluation: apply zigc to first argument only *)
let zp = zigc (square 2.)
let z = zp 1.

(* can also use labels to specify parameters *)
let zigcl ~x ~y = x +. 2. *. y

(* don't need labels if order of arguments is the same *)
let z = zigcl (square 2.) 1.

(* but can also do in opposite order with labels *)
let z = zigcl ~y:(square 2.) ~x:1.

(* partial eval with one arg, specify which with label *)
let zp = zigcl ~x:(square 2.)
let z = zp 1.

let zp = zigcl ~y:(square 2.)
let z = zp 1.

(* lists - sequences of arbitrary length, unlike tuples/records which are fixed length *)
[1; 2]

(* single type of element *)
[1; "a"]  (* error - can't have heterogeneous lists *)

[]  (* empty list *)

(* cons, ::, takes element and list and returns list with element at front *)
1 :: 2 :: [] = [1; 2]
1 :: 2  (* error, second arg not list *)

(* concat, @, takes two lists and concatenates, must copy first list so can be slow *)
[1; 2] @ [3; 4] = [1; 2; 3; 4]

(* pattern matching on lists *)

(* Returns the length of lst *)
let rec length (lst : 'a list) : int =
  match lst with
  | [] -> 0
  | h :: t -> 1 + length t

let z = length ["a"; "b"; "c"]

(* return nth element of list -- note curried parameters *)
let rec nth (lst : 'a list) (n : int) : 'a =
  match lst with
  | h :: t -> if n = 0 then h else nth t (n - 1)
  | [] -> raise Not_found

let z = nth ["a"; "b"; "c"] 2

(* partial evaluation *)
let select = nth ["a"; "b"; "c"]

let z = select 2
