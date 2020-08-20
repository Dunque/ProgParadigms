open List;;

let rec qsort1 ord = function
[] -> []
| h::t -> let after, before = partition (ord h) t in
          qsort1 ord before @ h :: qsort1 ord after;;

(*No será buena esta implementación en el caso de ordenar una lista muy grande, se producirá un desbordamiento del stack*)

let rec qsort2 ord = function
[] -> []
| h::t -> let after, before = partition (ord h) t in
                            rev_append (rev (qsort2 ord before)) (h :: qsort2 ord after);;

(*qsort2 es recursiva terminal ya que utiliza funciones recursivas terminales del módulo List, por lo que no desbordará el stack con listas muy grandes*)

let time f x=
    let t = Sys.time()
    in let fx = f x
    in Printf.printf "Tiempo: %fs\n" (Sys.time() -. t);;

let init n bound =
  let rec init_aux acc i n bound =
    if i >= n then acc
    else init_aux ((Random.int bound) :: acc) (i+1) n bound
  in init_aux [] 0 n bound;;

let tam = 1000000;;
let bound = 1000000;;
(*Listas de un millón de elemetos aleatorios del 0 al 1000000*)

let l1 = init tam bound;;
(*esta lista desborda el stack al hacerle qsort1*)

(*time (qsort1 (<)) l1;;*)

(*time (qsort2 (<)) l1;;*)

(*Medir tiempos*)
let tam = 100000;;
let bound = 100000;;

let l2 = init tam bound;;

time (qsort1 (<)) l2;;
(*Tiempo: 0.175781s*)
time (qsort2 (<)) l2;;
(*Tiempo: 0.188947s*)

(*Segun los resultados, qsort2 es un 7.5% mas lento que qsort1*)

let qsort3 ord l =
	let rec sort_asc = 
		function[] -> []
		| h::t -> let after, before = partition (ord h) t in rev_append (sort_des before) (h :: sort_asc after)
	and sort_des = 
		function[] -> []
		| h::t -> let before, after = partition (ord h) t in rev_append (sort_asc before) (h :: sort_des after)
	in sort_asc l;;

time (qsort3 (<)) l2;;
(*Tiempo: 0.186753s*)

(*Podemos observar que los tiempos de qsort3 y qsort2 son muy similares, y su diferencia con respecto a qsort1 se puede considerar igual que
la diferencia entre qsort1 y qsort2*)