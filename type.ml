type point = int * int
type vector = int * int

let sq x:int = x * x

let dist ((a1, a2):point) ((b1, b2):point) =
  let sum = (sq(b1 - a1) + sq(b2 - a2))
  in sqrt (float_of_int sum)

(* using actual `point` types - this works *)
let a = (1, 1)
let b = (2, 2);;

dist a b;;

(* using tuples - this works *)
dist (1, 1) (2, 2);;

(* using vector - wrong type, but it works too *)
let u = (1, 1)
let v = (2, 2);;

dist u v;;

(* interesting - I believe SML would have rejected the last expression *)
