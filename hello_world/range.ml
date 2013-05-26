(* Gives the range of integers between a and b *)
(* PART 1 - with currying *)
let rec range_1 a b =
  if a > b then []
  else a :: range_1 (a+1) b;;

(* cannot use `function` here, since that is just a special syntax for `match` *)
(* cannot use `fun` here, because recursive functions require a reference, but
 * fun is anonymous *)

let rec range_4 = fun a b ->
  if a > b then []
  else a :: range_4 (a+1) b;;

(* PART 2 - without currying *)
let rec range_2 (a, b) =
  if a > b then []
  else a :: range_2 (a+1, b);;

let rec range_3 = fun (a, b) ->
  if a > b then []
  else a :: range_3 (a+1, b);;

let result : int list = range_1 0 10;;
List.iter (Printf.printf "%d ") result;;
print_newline ();;

let result : int list = range_2 (0, 10);;
List.iter (Printf.printf "%d ") result;;
print_newline ();;

let result : int list = range_3 (0, 10);;
List.iter (Printf.printf "%d ") result;;
print_newline ();;

let result : int list = range_4 0 10;;
List.iter (Printf.printf "%d ") result;;
print_newline ();;
