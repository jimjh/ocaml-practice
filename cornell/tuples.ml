(* functions *)
let f((x:int),(y:int)):int = x+y;;
let rec fact(n:int):int = 
  if n=0 then 1
         else n*fact(n-1);;

f(2+3,5*6);;
fact(2*2);;

(* variables *)
let x=1;;
let f(y:int):int=x+y;;
let x=2;;
x;;
f(0);;


(* tuples *)
let max1((r1:float), (r2:float)):float =
  if r1 < r2 then r2
             else r1;;
max1(3.1415, 2.718);;

(3.1415, 2.718);;

(42, "hello", true);;
();;

let args = (3.1415, 2.718);;
max1 args;;

fst (1,2);;

let max2(pair:float*float):float =
    if (fst pair) < (snd pair) then
  (snd pair) else (fst pair);;
max2(3.,4.5);;

(* Pattern matching tuples *)

let max3(pair:float*float):float =
  let (r1, r2) = pair in
    if r1 < r2 then r2 else r1;;

let max1((r1:float), (r2:float)):float =
  if r1 < r2 then r2
             else r1;;
max1(3.1415, 2.718);;


let minmax(a, b):float*float =
  if a < b then (a, b) else (b, a);;
let (mn, mx) = minmax(2.0, 1.0);;


(* Records *)
type account = {first:string; last:string;
                age:int; balance:float};;

let pers1={first = "John"; last = "Doe";
           age = 150; balance = 0.12};;
pers1.age;;

let pers2={last = "Smith"; first = "Joe";
           balance = 120.56; age = 22};;


let {first=first0; last=last0;
     age=age0; balance=balance0} = pers2;;

let full_name{first=first;last=last;age=age;balance=balance}:string =
      first ^ " " ^ last (* ^ is the string concatenation operator *)


(* Datatypes *)
type mybool = Mytrue | Myfalse;;
type bool = true | false;;

type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat;;

(* maps a number to a day of the week *)
let int_to_day(i: int):day =
  if i mod 7 = 0 then Sun else
  if i mod 7 = 1 then Mon else
  if i mod 7 = 2 then Tue else
  if i mod 7 = 3 then Wed else
  if i mod 7 = 4 then Thu else
  if i mod 7 = 5 then Fri else Sat;;

let int_to_day2(i: int):day =
  (match i mod 7 with
     0 -> Sun
   | 1 -> Mon
   | 2 -> Tue
   | 3 -> Wed
   | 4 -> Thu
   | 5 -> Fri
   | _ -> Sat);;

(*
switch (i % 7) {
  case 0: return Sun;
  case 1: return Mon;
  case 2: return Tue;
  case 3: return Wed;
  case 4: return Thu;
  case 5: return Fri;
  default: return Sat;
}
*)

let day_to_int(d: day):int =
  (match d with
     Sun -> 0
   | Mon -> 1
   | Tue -> 2
   | Wed -> 3
   | Thu -> 4
   | Fri -> 5
   | Sat -> 6);;



(* Algebraic datatypes *)
type num = Int_num of int | Real_num of float;;

let num_to_float(n:num):float =
  (match n with
     Int_num(i) -> float_of_int(i)
   | Real_num(r) -> r);;
num_to_float(Int_num 1);;
num_to_float(Real_num 2.13);;

let max1(n1, n2):num =
  let r1:float = num_to_float(n1) in
  let r2:float = num_to_float(n2) in
    if r1 >= r2 then Real_num(r1) 
                else Real_num(r2);;

let max2(n1, n2):num =
  let r1:float = num_to_float(n1) in
  let r2:float = num_to_float(n2) in
    Real_num(if r1 >= r2 then r1 else r2);;

let rec max3(n1, n2):num =
  match (n1, n2) with
     (Real_num(r1), Real_num(r2)) ->
       Real_num(max r1 r2)
   | (Int_num(i1), Int_num(i2)) ->
       Int_num(max i1 i2)
   | (_, Int_num(i2)) -> 
       max3(n1, Real_num(float_of_int(i2)))
   | (Int_num(i1), _) -> 
       max3(n2, Real_num(float_of_int(i1)));;



let rec max4(n1, n2):num =
  match (n1, n2) with
     (Int_num(i1), Int_num(i2)) ->
       Int_num(max i1 i2)
   | (_, Int_num(i2)) -> 
       max4(n1, Real_num(float_of_int(i2)))
   | (Int_num(i1), _) -> 
       max4(n2, Real_num(float_of_int(i1)));;


let rec max5(n1, n2):num =
  match(n1, n2) with
     (Real_num(r1), Real_num(r2)) ->
       Real_num(max r1 r2)
   | (Int_num(i1), Int_num(i2)) ->
       Int_num(max i1 i2)
   | (_, Int_num(i2)) -> 
       max5(n1, Real_num(float_of_int(i2)))
   | (Int_num(i1), _) -> 
       max5(n2, Real_num(float_of_int(i1)))
   | (_, _) -> n1;;
