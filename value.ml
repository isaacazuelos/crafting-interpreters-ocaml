type type_name = Bool | Nil | Number | String | Func

exception TypeError of { expected: type_name; found: type_name } 

type t = 
  | LoxBool   of bool
  | LoxNil
  | LoxNumber of float
  | LoxString of string
  | LoxFunc   of lox_function
and lox_function = { arity: int; callable: t list -> t }

let typeof value = match value with
  | LoxBool   _ -> Bool
  | LoxNumber _ -> Number
  | LoxString _ -> String
  | LoxNil      -> Nil
  | LoxFunc   _ -> Func

let string_of_type_name tn = match tn with
  | Bool   -> "bool"
  | Nil    -> "nil"
  | Number -> "number"
  | String -> "string"
  | Func   -> "function"

let string_of value = match value with
  | LoxBool   b -> string_of_bool b
  | LoxNil      -> "nil"
  | LoxNumber n -> string_of_float n
  | LoxString s -> s
  | LoxFunc   r -> Printf.sprintf "<fun of %d args>" r.arity

let unary_minus value =
  match value with
  | LoxNumber n -> LoxNumber (~-. n)
  | v -> raise (TypeError { expected = Number; found = typeof v })

let truthy value = 
  match value with 
  | LoxBool b -> b
  | LoxNil -> false
  | _ -> true

let unary_bang value = LoxBool (truthy value)

let numeric_binop op left right = 
  match (left, right) with
  | (LoxNumber l, LoxNumber r) -> op l r
  | (LoxNumber _, v) -> raise (TypeError { expected = Number; found = typeof v })
  | (v, _)           -> raise (TypeError { expected = Number; found = typeof v })

(* gets a bit fancier because of overloading *)
let binary_plus left right =
  match (left, right) with
  | (LoxNumber l, LoxNumber r) -> LoxNumber (l +. r)
  | (LoxString l, LoxString r) -> LoxString (l ^ r)
  | (LoxString _, v) -> raise (TypeError { expected = String; found = typeof v })
  | (LoxNumber _, v) -> raise (TypeError { expected = Number; found = typeof v })
  (* Our error here isn't as nice... *)
  | (v, _)           -> raise (TypeError { expected = Number; found = typeof v })

let binary_minus  left right = LoxNumber (numeric_binop ( -. ) left right)
let binary_slash  left right = LoxNumber (numeric_binop ( /. ) left right)
let binary_star   left right = LoxNumber (numeric_binop ( *. ) left right)

let greater left right = 
  let op l r = l >  r in 
  LoxBool (numeric_binop op left right)

let greater_equal left right = 
  let op l r = l >= r in 
  LoxBool (numeric_binop op left right)

let less left right = 
  let op l r = l <  r in 
  LoxBool (numeric_binop op left right)

let less_equal left right = 
  let op l r = l <= r in 
  LoxBool (numeric_binop op left right)

let bang_equal  left right = LoxBool (left != right)
let equal_equal left right = LoxBool (left  = right)

let call value args =
  match value with
  | LoxFunc { callable = f; _ } -> f args
  | v -> raise (TypeError { expected = Func; found = typeof v })

let of_ocaml arity f = LoxFunc { arity; callable = f}

let arity value = 
  match value with 
  | LoxFunc r -> r.arity
  | _ -> 0