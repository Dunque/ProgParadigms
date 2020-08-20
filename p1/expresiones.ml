();;
(* unit = ()*)

2 + 5 * 3;;
(*int = 17*)

1.0;;
(*float = 1.*)

(*1.0 * 2;;
	Error: This expression has type float but an expression was expected of type
         int*)
1.0 *. 2.0;;
(*float = 2.*)

(*2 - 2.0;;
	Error: This expression has type float but an expression was expected of type
         int*)
2.0 -. 2.0;;
(*float = 0.*)

(*3.0 + 2.0
	Error: This expression has type float but an expression was expected of type
         int*)
3.0 +. 2.0;;
(*float = 5.*)

5 / 3;;
(*int = 1*)

5 mod 3;;
(*int = 2*)

3.0 *. 2.0 ** 3.0;;
(*float = 24.*)

3.0 = float_of_int 3;;
(*bool = true*)

(*sqrt 4;;
	Error: This expression has type int but an expression was expected of type
         float*)
sqrt 4.;;
(*float = 2.*)

int_of_float 2.1 + int_of_float (-2.9);;
(*int = 0*)

truncate 2.1 + truncate (-2.9);;
(*int = 0*)

floor 2.1 +. floor (2.9);;
(*float = 4.*)

(*ceil 2.1 +. ceil -2.9;;
	Error: This expression has type float -> float
       but an expression was expected of type float*)
ceil 2.1 +. ceil (-2.9);;
(*float = 1.*)

'B';;
(*char = 'B'*)

int_of_char 'A';;
(*int = 65*)

char_of_int 66;;
(*char = 'B'*)

Char.code 'B';;
(*int = 66*)

Char.chr 67;;
(*char = 'C'*)

'\067';;
(*char = 'C'*)

Char.chr (Char.code 'a' - Char.code 'A' + Char.code '�');;
(*char = '\241'*)

(*Char.uppercase '�';;
	Warning 3: deprecated: ISO-Latin1 characters in identifiers
	Error: Syntax error*)

Char.uppercase_ascii '�';;
(*char = '\209'*)

(*Char.lowercase 'O';;
	Warning 3: deprecated: Char.lowercase
	Use Char.lowercase_ascii instead.
	char = 'o'*)
Char.lowercase_ascii 'O';;
(*char = 'o*)

"this is a string";;
(*string = "this is a string"*)

String.length "longitud";;
(*int = 8*)

(*"1999" + "1";;
	Error: This expression has type string but an expression was expected of type
         int*)
int_of_string "1999" + int_of_string "1";;
(*int = 2000*)

"1999" ^ "1";;
(*string = "19991"*)

int_of_string "1999" + 1;;
(*int = 2000*)

"\064\065";;
(*string = "@A"*)

string_of_int 010;;
(*string = "10"*)

not true;;
(*bool = false*)

true && false;;
(*bool = false*)

true || false;;
(*bool = true*)

(1 < 2) = false;;
(*bool = false*)

"1" < "2";;
(*bool = true*)

2 < 12;;
(*bool = true*)
"2" < "12";;
(*bool = false*)

"uno" < "dos";;
(*bool = false*)

2,5;;
(*int * int = (2, 5)*)

"hola", "adios";;
(*string * string = ("hola", "adios")*)

0, 0.0;;
(*int * float = (0, 0.)*)

fst ('a',0);;
(*char = 'a'*)

snd (false,true);;
(*bool = true*)

(1,2,3);;
(*int * int * int = (1, 2, 3)*)

(1,2),3;;
(*int * int) * int = ((1, 2), 3)*)

fst ((1,2),3);;
(*int * int = (1, 2)*)

if 3=4 then 0 else 4;;
(*int = 4*)

if 3=4 then "0" else "4";;
(*string = "4"*)

(*if 3=4 then 0 else "4";;
	Error: This expression has type string but an expression was expected of type
         int*)
if 3=4 then 0 else int_of_string "4";;
(*int = 4*)

(if 3 < 5 then 8 else 10) + 4;;
(*int = 12*)

2.0 *. asin 1.0;;
(*float = 3.14159265358979312*)

sin (2.0 *. asin 1.0 /. 2.);;
(*float = 1.*)

function x -> 2 * x;;
(*int -> int = <fun>*)

(function x -> 2 * x) (2 + 1);;
(*int = 6*)

function (x,y) -> 2 * x + y;;
(*int * int -> int = <fun>*)

(function (x,y) -> 2 * x + y) (1 + 2, 3);;
(*int = 9*)