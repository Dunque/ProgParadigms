open List;;

(*Inicializadores de lista y medidor de tiempo*)
let time f x=
    let t = Sys.time()
    in let fx = f x
    in Printf.printf "Tiempo: %fs\n" (Sys.time() -. t);;

let init n bound =
  let rec init_aux acc i n bound =
    if i >= n then acc
    else init_aux ((Random.int bound) :: acc) (i+1) n bound
  in init_aux [] 0 n bound;;

let tam = 100000;;
let bound = 100000;;
(*Listas de un millón de elemetos aleatorios del 0 al 100000*)

let l1 = init tam bound;;

let tam = 10000000;;
let bound = 10000000;;
(*Listas de un millón de elemetos aleatorios del 0 al 10000000*)

let l2 = init tam bound;;

let rec split = function
    [] -> [], []
    | h::[] -> [h],[]
    | h1::h2::t -> let t1,t2 = split t
                   in (h1::t1, h2::t2);;

let rec merge ord l1 l2 = 
    match l1,l2 with
        [],l | l,[] -> l1
        | h1::t1, h2::t2 -> if ord h1 h2 then h1::merge ord t1 l2
                            else h2::merge ord l1 t2;;

(*La no terminalidad de split y merge hace que con listass del tamaño de l2 (1000000 elementos) den un rebasamiento del stack*)

let split_t l = 
   let rec _split l right left =
      match l with
        | [] -> (List.rev left, right)
        | h::t-> _split t left (h:: right) 
   in _split l [] [];;

let merge_t ord l1 l2 = 
   let rec loop ord l1 l2 acc =
      match l1, l2 with
        | [], [] -> acc
        | [], h :: t | h :: t, [] -> loop ord [] t (h :: acc)
        | h1 :: t1, h2 :: t2 ->
            if ord h1 h2 then loop ord t1 l2 (h1 :: acc)
            else loop ord l1 t2 (h2 :: acc)
   in List.rev (loop ord l1 l2 []);;


let rec msort1 ord l = 
    match l with
        [] | _::[] -> l
        | _ -> let l1, l2 = split l 
                in merge ord (msort1 ord l1) (msort1 ord l2);;

let rec msort2 ord l = 
    match l with
      [] | _::[] -> l
      | _ -> let l1, l2 = split_t l
              in merge_t ord (msort2 ord l1) (msort2 ord l2);;


let qsort3 ord l =
	let rec sort_asc = 
		function[] -> []
		| h::t -> let after, before = partition (ord h) t in rev_append (sort_des before) (h :: sort_asc after)
	and sort_des = 
		function[] -> []
		| h::t -> let before, after = partition (ord h) t in rev_append (sort_asc before) (h :: sort_des after)
	in sort_asc l;;

(* Medir tiempos *)

time (msort1 (<)) l1;;
(*Tiempo: 0.095633s*)
time (msort2 (<)) l1;;
(*Tiempo: 0.196958s*)
time (qsort3 (<)) l1;;
(*Tiempo: 0.198203s*)

(*El tiempo de ejecución de msort1 es aproximadamente la mitad que de msort2 y qsort3, los cuales son muy similares. Cabe decir que
msort1 no es terminal, y las otras dos sí.*)