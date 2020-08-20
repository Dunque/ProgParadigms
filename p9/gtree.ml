type 'a g_tree = Gt of 'a * 'a g_tree list;;

let x = Gt (1,[Gt (2,[])]);;

let rec breadth_first = function
Gt (x, []) -> [x]
| Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;

let breadth_first_t tree = 
      let rec loop tree a = 
      match tree with
       Gt (x, []) -> (List.rev_append a [x])
       | Gt (x, (Gt (y, t2))::t1) -> loop (Gt (y, List.rev_append (List.rev t1) t2)) (x::a)
      in loop tree [];;

let rec g_tree_perfect n =
  if (n<=0) then Gt (0,[])
  else let aux = g_tree_perfect (n-1)
        in Gt (n,[aux;aux]);;

let t = g_tree_perfect 20;;