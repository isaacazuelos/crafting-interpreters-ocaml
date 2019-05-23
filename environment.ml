open Error

type t = { 
  enclosing: t option;
  values: (string, Value.t) Hashtbl.t;
}

let create ?enclosing () = { values = Hashtbl.create 64; enclosing }

let define env name value = Hashtbl.replace env.values name value

let rec get env line name = 
  match Hashtbl.find_opt env.values name with
  | Some v -> v
  | None   -> 
    match env.enclosing with
    | Some e -> get e line name
    | None   -> raise (RuntimeError {
        where   = line; 
        message = "Undefined variable '" ^ name ^ "'."
      })

let rec assign env line name value =
  match Hashtbl.find_opt env.values name with
  | Some _ -> Hashtbl.replace env.values name value; value
  | None   -> 
    match env.enclosing with
    | Some e -> assign e line name value
    | None   -> raise (RuntimeError { 
        where = line;
        message = "Undefined variable '" ^ name ^ "'." })
