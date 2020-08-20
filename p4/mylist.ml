let hd = function
  h::_ -> h
  | [] -> failwith "hd";;

let tl = function
  _::t -> t
  | [] -> failwith "tl";;

let rec length = function
	[] -> 0
	| _::t -> 1 + length t;;

let rec compare_lengths = function
	[] -> (function [] -> 0
		   		| h::t -> -1)
	| h::t -> (function [] -> 1
					| h2::t2 -> compare_lengths t t2);;

let rec nth l n =
	match l with
  [] -> failwith "nth"
  | a::l -> if n = 0 then a else nth l (n-1);;

let rec append l1 l2 =
	match l1 with
	[] -> l2;
	| h::t -> h::append t l2;;

(*-------------------------------------------------------------*)

let init n f =
  let rec init_aux i n f =
    if i >= n then []
    else (f i) :: (init_aux (i+1) n f)
  in init_aux 0 n f;;

let rec rev = function
  [] -> []
	| x::l -> (rev l) @ [x];;

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a::l -> rev_append l (a::l2);;

let rec concat = function
    [] -> []
  | l::r -> l @ concat r;;

let flatten = concat;;

let rec map f = function 
  [] -> []
	| x::l -> (f x)::(map f l);;

let rev_map f l =
  let rec rmap_f accu = function
    | [] -> accu
    | a::l -> rmap_f (f a :: accu) l
in rmap_f [] l;;

let rec map2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a1::l1, a2::l2) -> let r = f a1 a2
    in r::map2 f l1 l2
  | (_, _) -> failwith "map2";;

let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::l -> fold_left f (f accu a) l;;

let rec fold_right f l accu =
  match l with
    [] -> accu
  | a::l -> f a (fold_right f l accu);;

(*-------------------------------------------------------------*)

let rec find p = function
  | [] -> raise Not_found
  | x::l -> if p x then x else find p l;;

let rec for_all p = function
    [] -> true
  | a::l -> p a && for_all p l;;


let rec exists p = function
    [] -> false
  | a::l -> p a || exists p l;;


let rec mem x = function
    [] -> false
  | a::l -> compare a x = 0 || mem x l;;

let filter p =
  let rec find accu = function
  | [] -> rev accu
  | x::l -> if p x then find (x::accu) l else find accu l
in find [];;

let find_all = filter;;

let partition p l =
  let rec part yes no = function
  | [] -> (rev yes, rev no)
  | x::l -> if p x then part (x::yes) no l else part yes (x::no) l
in part [] [] l;;

let rec split = function
    [] -> ([], [])
  | (x,y)::l ->
      let (rx, ry) = 
        split l
      in (x::rx, y::ry);;

let rec combine l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a::l1, b::l2) -> (a, b) :: combine l1 l2
  | (_, _) -> failwith "combine";;