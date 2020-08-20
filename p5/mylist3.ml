let remove n l =
    let rec aux n l acc=
        match l with
            [] -> []
            | h::t -> if h = n then acc @ t else aux n t (acc @ [h])
    in aux n l [];;

let rec remove_all n l =
    match l with
        [] -> []
        | h::t -> if h = n then remove_all n t else h::(remove_all n t);;

let rec ldif l1 l2 = 
    match (l1,l2) with
        (l1,[]) -> l1
        | ([], l2) -> l1
        | h1::t1, h2::t2 -> ldif (remove_all h2 l1) t2;;
    
let lprod l1 l2 =	
	let rec aux acc = function
		[],_ -> rev acc
		| _::t1,[] -> aux acc (t1,l2)
		| h1::t1,h2::t2 -> aux ((h1,h2)::acc) (h1::t1,t2)
	in aux [] (l1,l2);;

let divide l =
    let rec aux acc acc2 = function
		[h] -> (rev (h::acc), rev acc2)
		| [] -> (rev acc, rev acc2)
		| h::t -> aux (h::acc) (hd t::acc2) (tl t)
	in aux [] [] l;;

let comp = function f -> function y -> function x -> f(y(x));;


