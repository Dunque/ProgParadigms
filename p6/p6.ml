let split l = 
	List.map (function (x,y) -> x) l, List.map (function (x,y) -> y) l;;

let combine l1 l2 = 
	List.map2 (function x -> function y -> x,y) l1 l2;;


let length l1 = 
	List.fold_right (function _->(+)1) l1 0;;

let append l1 l2 =
	List.fold_right (function h -> function t -> h::t) l1 l2;;

let rev l = 
	List.fold_left (function t -> function h -> h::t) [] l;;


let concat l =
	List.fold_right (function h -> function t -> h @ t)  l [];;


let partition p l = 
	(List.filter p l), (List.filter (function x -> not (p x)) l);;

let remove_all n l =
	List.filter (function x -> n != x) l;;


let ldif l1 l2 =
	List.fold_right remove_all l2 l1;;

let lprod l1 l2=
    List.concat (List.map (function h-> List.map (function h2 -> (h,h2)) l2) l1);;

let comp = function f -> function y -> function x -> f(y(x));;

let multicomp l=
    List.fold_right (fun x aux -> comp x aux ) l (function x -> x);;