open Regexp;;

let regexp_of_string s = Regexp_parser.main Regexp_lex.token(Lexing.from_string s);;

let rec nullable = function
  Void | Expres _ | Except _ | Any -> Void
  | Empty -> Empty
  | Concat(r,s) | And (r,s) -> if nullable r  = Empty && nullable s = Empty then Empty else Void
  | Rep(r) -> Empty
  | Or(r,s) -> if (nullable(r) = Empty || nullable(s) = Empty) then Empty else Void;;

let rec derive a = function
  Void | Empty -> Void
  | Expres(One(r)) -> if (r = a) then Empty else Void
  | Except(One(r)) -> if (r = a) then Void else Empty
  | Expres(Range(r,s)) -> if (r <= a && a <= s) then Empty else Void
  | Except(Range(r,s)) -> if (r <= a && a <= s) then Void else Empty
  | Any -> Empty
  | Concat(r,s) -> Or(Concat(derive a r,s),Concat(nullable r, derive a s)) 
  | Rep(r) -> Concat(derive a r, Rep(r))
  | Or(r,s) -> Or(derive a r, derive a s)
  | And(r,s) -> And(derive a r, derive a s);;

let rec simplify = function
   Concat(r,s) -> (match simplify r, simplify s with
                         Void, _ | _, Void -> Void
                         | Empty,e | e,Empty -> e
                         | r',s' -> Concat(r', s'))
  | Rep(Empty) | Rep(Void) -> Empty
  | Rep(r) -> Rep(simplify r)
  | Or(r,s) -> (match simplify r, simplify s with
                         Void, e | e, Void -> e
                         | Empty,_ | _,Empty -> Empty
                         | r',s' -> Or(r', s'))
  | And(Void,r) | And(r,Void) -> Void
  | And(r,s) -> (match simplify r, simplify s with
                         Void, _ | _, Void -> Void
                         | Empty,e | e,Empty -> e
                         | r',s' -> And(r', s'))
  | r -> r;;

let matches_regexp s exp = 
  let rec aux count s exp =
    if (count = (String.length s)) then (nullable exp = Empty)
    else aux (count+1) s (simplify (derive (s.[count]) exp))
  in aux 0 s exp;;

let matches s1 s2 =
  matches_regexp s1 (regexp_of_string s2);;