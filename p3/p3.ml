let f = function x -> function y -> function z ->
    if (z > y) then true else if (x <> y) then (z / (x - y) > y) else false;;
    
false && (2 / 0 > 0);;
(* False *)

true && (2 / 0 > 0);;
(* False, exception: division by zero *)

true || (2 / 0 > 0);;
(* True *)

false || (2 / 0 > 0);;
(* False, exception: division by zero *)

let con = (&&);;
(* val con : bool -> bool -> bool = <fun> *)

let dis = (||);;
(* val dis : bool -> bool -> bool = <fun> *)

(&&) (1 < 0) (2 / 0 > 0);;
(* False *)

con (1 < 0) (2 / 0 > 0);;
(* Exception: division by zero *)

(||) (1 < 0) (2 / 0 > 0);;
(* Exception: division by zero *)

dis (1 < 0) (2 / 0 > 0);;
(* Exception: division by zero *)

let curry = function f -> function x -> function y -> f(x,y);;
(* val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c = <fun> *)

let uncurry = function f -> function(x,y) -> f(x)y;;
(* val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c = <fun> *)

uncurry(+);;
(* - : int * int -> int = <fun> *)

let sum = (uncurry(+));;
(* val sum : int * int -> int = <fun> *)

sum (1,0);;
(* sum 1;;*)
(* Error: expected expression of type int * int *)

sum (2,1);;
(* - : int = 3 *)

let g = curry(function p -> 2 * fst p + 3 * snd p);;
(* val g : int -> int -> int = <fun> *)

let h = g 2;;
(* val h : int -> int = <fun> *)

h 1, h 2, h 3;;
(* - : int * int * int = (7, 10, 13) *)
let f = function x -> function y -> function z -> float_of_int(x+y) +. z;;
let comp = function f -> function g -> function x -> f(g(x));;
(* val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun> *)

let f2 = let square x = x*x in comp square((+) 1);;
(* val f2 : int -> int = <fun> *)

f2 1, f2 2, f2 3;;
(* - : int * int * int = (4, 9, 16) *)