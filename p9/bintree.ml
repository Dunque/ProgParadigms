open List;;

type 'a bin_tree = Empty | Node of 'a * 'a bin_tree * 'a bin_tree;;


let tree = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty));;



let revAux l1 l2 = rev_append (rev l1) l2;;

let breadth_first tree =
    let rec aux t list queue = match (t, queue) with
        (Empty, []) -> list
        |(Empty, h::t) -> aux h list t
        |(Node (x, Empty, Empty), []) -> revAux list (x::[])
        |(Node (x, Empty, Empty), h::t) -> aux h (revAux list (x::[])) t
        |(Node (x, y, Empty), []) -> aux y (revAux list (x::[])) []
        |(Node (x, y, Empty), h::t) -> aux h (revAux list (x::[])) (revAux t (y::[]))
        |(Node (x, Empty, z), []) -> aux z (revAux list (x::[])) []
        |(Node (x, Empty, z), h::t) -> aux h (revAux list (x::[])) (revAux t (z::[]))
        |(Node (x, y, z), []) -> aux y (revAux list (x::[])) (z::[])
        |(Node (x, y, z), h::t) -> aux h (revAux list (x::[])) (revAux t (y::z::[]))
in aux tree [] [];;