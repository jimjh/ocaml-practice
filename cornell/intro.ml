let rec sumsq (n:int) : int =
  if n=0 then 0
  else n*n + sumsq(n-1)

let rec sumsq (n) =
  if n=0 then 0
  else n*n + sumsq(n-1)

let rec sumop (f,n) =
  if n=0 then 0
  else f n + sumop(f,(n-1));;

sumop((function x -> x*x*x), 5)

let rec reverse (lst:'a list): ' list =
  match lst with
      [] -> []
    | h :: t -> reverse t  @ [h]

let pythagoras (x, y, z) =
  let square n = n*n in
  square z = square x + square y;;

Printf.printf "%B\n" (pythagoras (1, 2, 3));;

Printf.printf "%B\n" (pythagoras (3, 4, 5));;

