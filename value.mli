type type_name
val string_of_type_name: type_name -> string

exception TypeError of { expected: type_name; found: type_name } 

type t = 
  | LoxBool   of bool
  | LoxNil
  | LoxNumber of float
  | LoxString of string
  | LoxFunc   of lox_function
and lox_function = { arity: int; callable: t list -> t }

val typeof: t -> type_name
val string_of: t -> string

val truthy: t -> bool

(* Unary operators *)
val unary_minus: t -> t
val unary_bang: t -> t

(* Math *)
val binary_plus: t -> t -> t
val binary_minus: t -> t -> t
val binary_slash: t -> t -> t
val binary_star: t -> t -> t

(* Comparisons *)
val greater: t -> t -> t
val greater_equal: t -> t -> t
val less: t -> t -> t
val less_equal: t -> t -> t

(* Equality *)
val bang_equal: t -> t -> t
val equal_equal: t -> t -> t

(* Function calls *)
val arity: t -> int
val of_ocaml: int -> (t list -> t) -> t
val call: t -> t list -> t