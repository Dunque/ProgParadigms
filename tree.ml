(* ÁRBOLES BINARIOS *)

type 'a bintree = Empty | Node of 'a * 'a bintree * 'a bintree;;

let rec nNodes = function
  Empty -> 0
  | Node (_,l,r) -> 1 + nNodes l + nNodes r;;

let rec height = function
  Empty -> 0
  | Node (_,l,r) -> 1 + max (height l) (height r);;

let leaf x = Node(x,Empty,Empty);;

let rec leaves = function
  Empty -> []
  | Node (x,Empty,Empty) -> [x]
  | Node (_,l,r) -> leaves l @ leaves r;;

let rec inorder = function
  Empty -> []
  | Node (x,l,r) -> inorder l @ x::inorder r;;

let rec mirror = function
  Empty -> Empty
  | Node (x,l,r) -> Node (x, mirror r, mirror l);;


(* ARBOLES FB *)

type 'a fbtree = 
  Leaf of 'a
  | Node of 'a * 'a fbtree * 'a fbtree;;


(* ARBOLES GENERICOS *)

type 'a g_tree =
  Gt of 'a * 'a g_tree list;;

let rec nNodes = function
  Gt (_, []) -> 1
  | Gt-> 1 + nNodes l + nNodes r;;
