let answer_g = [( (2, 2, 2), (1, 3, 3) )];;

let add (a, b, c) = a + b + c in
let split (a, b) = add a * add b in
let rec zardoz = function
    [] -> []
  | h :: t -> (split h :: zardoz t) in
List.hd (zardoz answer_g);;
