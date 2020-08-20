let f x = 
  let (x,y) = x
  in x;; 

let f (x,y) = x;;

let n x g =
  if g x then true
  else false;;

let n x g = g x;;

let sorted l =
  let rec sort l acc =
    match l with
    [] | [_] -> acc
    | x::y::z -> if x<=y then (sort [@tailcall]) (y::z) true
                 else false
  in sort l true;;
    