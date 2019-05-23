type t = { 
  enclosing: t option;
  values: (string, Value.t) Hashtbl.t;
}
val create : ?enclosing: t -> unit -> t

val define : t -> string -> Value.t -> unit
val get    : t -> int -> string -> Value.t
val assign : t -> int -> string -> Value.t -> Value.t
