(* Calculates the factorial of n *)
let rec fac = function
  | 0 -> 1
  | n -> n * fac (n-1);;

print_int (fac 5);;
