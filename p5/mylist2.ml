let hd = function
  h::_ -> h
  | [] -> failwith "hd";;

let tl = function
  _::t -> t
  | [] -> failwith "tl";;

let length l =
  let rec length_aux l len =
  match l with
    [] -> len
    | a::l -> length_aux l (len + 1)
  in length_aux l 0;;

let rec compare_lengths l1 l2 = 
  match (l1,l2) with
  ([],[]) -> 0
  | (h::t,[]) -> 1
  | [], h::t -> -1
  | (h1::t1, h2::t2) -> (compare_lengths [@tailcall]) t1 t2;;

let nth l n =
  if n < 0 then failwith "nth" else
  let rec nth_aux l n =
    match l with
      [] -> failwith "nth"
      | a::l -> if n = 0 then a else nth_aux l (n-1)
    in nth_aux l n

let rec append l1 l2 =
  match l1 with
  [] -> l2;
  | h::t -> h::append t l2;;

(*-------------------------------------------------------------*)

let init n f =
  let rec init_aux acc i n f =
    if i >= n then acc
    else init_aux ((f i) :: acc) (i+1) n f
  in init_aux [] 0 n f;;

let rev l =
  let rec rev_aux a = function
    [] -> a
    | h::t -> rev_aux (h::a) t
  in rev_aux [] l;;

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a::l -> (rev_append [@tailcall]) l (a::l2);;

let concat l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    [], [] -> List.rev acc
    | [], h :: t -> loop (h::acc) [] t
    | h :: t, l -> loop (h::acc) t l
    in loop [] l1 l2;;

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

let rec fold_left f acc l =
  match l with
    [] -> acc
  | h::t -> fold_left f (f acc h) t;;

let rec fold_right f l acc =
  match l with
    [] -> acc
  | h::t -> f h (fold_right f t acc);;

(*-------------------------------------------------------------*)

let rec find p = function
  | [] -> raise Not_found
  | h::t -> if p h then h else (find [@tailcall]) p t;;

let rec for_all p = function
    [] -> true
  | h::t -> p h && (for_all [@tailcall]) p t;;


let rec exists p = function
    [] -> false
  | h::t -> p h || (exists [@tailcall]) p t;;


let rec mem x = function
    [] -> false
  | h::t -> compare h x = 0 || (mem [@tailcall]) x t;;

let filter p =
  let rec find accu = function
  | [] -> rev accu
  | h::t -> if p h then find (h::accu) t else find accu t
in find [];;

let find_all = filter;;

let partition p l =
  let rec part yes no = function
  | [] -> (rev yes, rev no)
  | h::t -> if p h then part (h::yes) no t else part yes (h::no) t
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