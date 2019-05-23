type expr = 
  | Assign of assign
  | Binary of binary
  | Grouping of grouping
  | Literal of literal
  | Logical of binary
  | Unary of unary
  | Variable of Scanner.token
  | Call of call

and binary   = { left: expr; bin_op: Scanner.token; right: expr }
and grouping = { expression: expr }
and literal  = { token: Scanner.token; value: Value.t }
and unary    = { unary_op: Scanner.token; opperand: expr }
and assign   = { name: Scanner.token; new_value: expr }
and call     = { callee: expr; paren: Scanner.token; arguments: expr list }

let rec string_of expr = 
  let soe = string_of in
  match expr with
  | Assign   e -> "(" ^ e.name.lexem ^ " = " ^ soe e.new_value ^ ")"
  | Binary   e | Logical e -> "(" ^ soe e.left ^ " " ^ e.bin_op.lexem ^ " " ^ soe e.right ^ ")"
  | Grouping e -> "(" ^ soe e.expression ^ ")"
  | Literal  e -> Value.string_of e.value
  | Unary    e -> "(" ^ e.unary_op.lexem ^ " " ^ soe e.opperand ^ ")"
  | Variable e -> e.lexem
  | Call     e -> e.paren.lexem ^ "(" ^ String.concat ", " (List.map soe e.arguments) ^ ")"

let rec location expr = match expr with
  | Assign   r -> r.name
  | Binary e | Logical e -> e.bin_op
  | Grouping e -> location (e.expression)
  | Literal  e -> e.token
  | Unary    e -> e.unary_op
  | Variable t -> t
  | Call     e -> e.paren