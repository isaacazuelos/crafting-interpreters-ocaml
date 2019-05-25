open Expression

type stmnt = 
  | Expression of expr
  | Print of expr
  | VarDecl of var_decl
  | Block of block
  | IfStmnt of if_stmnt
  | While of while_stmnt
  | Function of prototype
  | Return of return
and var_decl    = { name: Scanner.token; initalizer: expr option }
and block       = { statements: stmnt list; open_brace: Scanner.token }
and if_stmnt    = { condition: expr; true_branch: stmnt; false_branch: stmnt option }
and while_stmnt = { loop_condition: expr; body: stmnt }
and prototype   = { func_name: Scanner.token; params: Scanner.token list; func_body: block }
and return      = { keyword: Scanner.token; value: expr option }

let location stmnt = 
  match stmnt with
  | Expression e -> location e
  | Print      e -> location e
  | VarDecl    r -> r.name
  | Block      r -> r.open_brace
  | IfStmnt    r -> location r.condition
  | While      r -> location r.loop_condition
  | Function   r -> r.func_name
  | Return     r -> r.keyword

let rec string_of s = 
  let sos = string_of in
  let soe = Expression.string_of in
  match s with 
  | Expression e -> soe e ^ ";"
  | Print      e -> "print " ^ soe e ^ ";"
  | VarDecl    r -> let init = match r.initalizer with 
      | None   -> ";" 
      | Some e -> " = " ^ soe e ^ ";"
    in 
    "var " ^ r.name.lexem ^ init
  | Block      r ->  
    let ss = List.map sos r.statements in
    let body =  String.concat ", " ss in
    "{ " ^ body ^ " }"
  | IfStmnt   r -> 
    let suffix = match r.false_branch with 
      | None -> "" 
      | Some s -> " else " ^ sos s in
    "if (" ^ soe r.condition ^ ") " ^ sos r.true_branch ^ suffix
  | While    r -> "while (" ^ soe r.loop_condition ^ ") " ^ sos r.body
  | Function r ->
    let name = r.func_name.lexem in
    let params = String.concat ", " (List.map (fun (p: Scanner.token) -> Scanner.lexem p) r.params) in
    let body = "{ " ^ String.concat ", " (List.map sos r.func_body.statements) ^ " }" in
    "fun " ^ name ^ " (" ^ params ^ ") " ^ body
  | Return r -> "return" ^ match r.value with
    | None -> ";" 
    | Some e -> soe e ^ ";"