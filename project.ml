type ide = string;;
  
type loc = int;;

type exp = 
        Eint of int 
      | Ebool of bool 
      | Echar of char
      | Empty
      | Cons of exp * exp
      | Den of ide
      | Prod of exp * exp
      | Sum of exp * exp
      | Diff of exp * exp
      | Mod of exp * exp
      | Div of exp * exp
      | Lessint of exp * exp
      | Eqint of exp * exp
      | Iszero of exp
      | Lesschar of exp * exp
      | Eqchar of exp * exp
      | Or of exp * exp
      | And of exp * exp
      | Not of exp
      | Ifthenelse of exp * exp * exp
      | Let of (ide * exp) list * exp      
      | Fun of ide list * exp
      | Apply of exp * exp list;;

  
type generic = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z;;

  
type typ = 
    Int 
  | Bool
  | Char
  | List of typ
  | Fun of typ * typ
  | Gen of generic;;

  
let type_inf expr =
  match expr with
    Eint (n) -> Int
  | Ebool (b) -> Bool
  | Echar (c) -> Char
  | Cons (a1, a2) -> if type_inf a1 = type_inf a2 then List (type_inf a1)
		     else failwith "error";;

  
let semprod (a, b) =
  match a, b with
    Eint (a'), Eint (b') -> Eint (a' * b');;

let semsum (a, b) =
  match a, b with
    Eint (a'), Eint (b') -> Eint (a' + b');;

  
let semdiff (a, b) =
  match a, b with
    Eint (a'), Eint (b') -> Eint (a' - b');;

  
let semmod (a, b) =
  match a, b with
    Eint (a'), Eint (b') when b' != 0 -> Eint (a' mod b')
  | Eint (a'), Eint (b') when b' = 0 -> failwith "error";;

  
let semdiv (a, b) =
  match a, b with
    Eint (a'), Eint (b') when b' != 0 -> Eint (a' / b')
  | Eint (a'), Eint (b') when b' = 0 -> failwith "error";;

  
let sem expr =
  match expr with
    Prod (a, b) -> semprod (a, b)
  | Sum (a, b) -> semsum (a, b)
  | Diff (a, b) -> semdiff (a, b)
  | Mod (a, b) -> semmod (a, b)
  | Div (a, b) -> semdiv (a, b);;
