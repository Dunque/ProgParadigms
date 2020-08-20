type symbol =
  One of char
  | Range of char*char;;

type regexp = 
  Void 
  | Empty 
  | Expres of symbol
  | Concat of regexp*regexp 
  | Rep of regexp 
  | Or of regexp*regexp 
  | And of regexp*regexp 
  | Any 
  | Except of symbol;;

let symbol_of_char x = One(x);;
let symbol_of_range x y = Range(x,y);;

let empty = Void;;
let empty_string = Empty;;

let single = function x -> Expres(x);;
let except = function x -> Except(x);;
let any = Any;;
let concat = fun x y -> Concat(x,y);;
let repeat = fun x -> Rep(x);;
let alt = fun x y -> Or(x,y);;
let all = fun x y -> And(x,y);;