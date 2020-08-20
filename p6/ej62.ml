let f x = x;;

let g x = (x,x);;

let h (x,y) = x;;

let i (x,y) = y;;

let j x = [x];;

(* Para las cuatro primeras solo se puede escribir una funcion asi, minstras que para j se pueden escribir infinitas *)

let rec k x = k x;;
let k x = raise Not_found;;

let l x = [];;

(* l se puede hacer de infinitas formas, haciendo [] @ [] concatenaciones infinitas de la lista vacía*)


(*Es imposible definir una función como fun_123_ab porque se producirían errores de tipo durante la ejecución *)