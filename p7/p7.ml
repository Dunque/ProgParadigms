let suml l =
    let rec aux acc = function
      | [] -> acc
      | h::t -> aux (h + acc) t in
      aux 0 l;;


let maxl l =
	match l with
		[] -> raise (Failure "maxl")
		| h::t -> let rec aux n = function
					[] -> n
					| h::t -> if h>n then aux h t else aux n t
				in aux h t;;



let to0from num =
     let rec helper = fun n acc ->
          if n < 0
          then List.rev(acc)
          else helper (n - 1) (n :: acc)
     in
     helper num [];;


let rec fromto m n =
if m > n then []
else m :: fromto (m+1) n;;


let fromto num mun =
     let rec aux = fun n m acc ->
          if n > m
          then List.rev acc
          else aux (n + 1) m (n :: acc)
     in
     aux num mun [];;


let from1to num =
     let rec helper = fun n acc ->
          if n < 1
          then acc
          else helper (n - 1) (n :: acc)
     in
     helper num [];;


let append l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
    in
    loop [] l1 l2;;


let map f l =
  let rec map_aux acc = function
    | [] -> acc []
    | x :: xs -> map_aux (fun ys -> acc (f x :: ys)) xs
  in
  map_aux (fun ys -> ys) l;;


let power x y =
  let rec loop = fun acc x y ->
      if y = 1 then x
      else loop acc (acc*x) (y-1) in
     loop x x y;;
      

	let fold_right f l b =
	  let rev = List.rev l 
	  in 
	  let rec folder_rr acc = function
	    | [] -> acc
	    | hd::tl -> folder_rr (f hd acc) tl
	  in 
	  folder_rr b rev;;

let incseg l = fold_right (fun x t -> x::map ((+) x) t) l [];;


let remove x l =
     let rec loop x l=
          match l with 
            [] -> l
            | h::t -> if h = x then t
                                else loop  x t in
                    loop x l;;
          

let insert x l =
     let rec loop x l a =
          match l with 
            [] -> (List.rev_append a [x])
            | h::t -> if x<= h then List.rev_append a (x::h::t)
               else loop x t (h::a) in 
            loop x l [];;


let rec insert_gen f x l = match l with
[] -> [x]
| h::t -> if f x h then x::l
              else h :: insert_gen  f x t;;

let insert_gen f x l =
     let rec loop f x l = 
            match l with 
               [] -> [x]
               | h::t -> if f x h then x::l 
               else h :: loop f x t in
               loop f x l;;




