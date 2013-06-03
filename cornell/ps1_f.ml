let answer_f = [ 0; 0; 0; 0; 0; 0; ];;

let foo a b = a - b + 7 in
let rec zardoz z =
  match z with
      x :: y :: z :: t -> foo x y + foo y z + foo z x + zardoz t
    | [] -> 0
    | _ -> failwith "sorry" in
zardoz answer_f;;
