print_string "Hello World";

(* This is a comment *)

type row = int list
type students = row array

(* Purpose: add up the numbers in the row *)
let sum (r : row) : int =
  let rec aux n = function
    | [] -> n
    | h::t ->  aux (h + n) t
  in aux 0 r;;
