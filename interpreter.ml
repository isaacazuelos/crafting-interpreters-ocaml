open Scanner
open Parser
open Expression
open Error

type t = { 
  mutable env: Environment.t; 
  mutable globals: Environment.t;
}

exception Return of Value.t

let create () = 
  let globals = Environment.create () in
  let state = { globals; env = globals; } in

  (* make sure to capture state if you need it -- it won't be passed back. *)
  let clock _args = Value.LoxNumber (Unix.gettimeofday ()) in
  Environment.define globals "clock" (Value.of_ocaml 0 clock);

  state

let rec eval state expr =
  match expr with
  | Literal e  -> e.value
  | Grouping e -> eval state e.expression
  | Unary e -> (match e.unary_op.token_type with
      | Bang  -> Value.unary_bang  (eval state e.opperand)
      | Minus -> Value.unary_minus (eval state e.opperand)
      | _ -> raise (RuntimeError { where = e.unary_op.line; message = "invalid unary operator"} ))
  | Binary e -> (match e.bin_op.token_type with 
      (* math *)
      | Minus        -> Value.binary_minus  (eval state e.left) (eval state e.right)
      | Slash        -> Value.binary_slash  (eval state e.left) (eval state e.right)
      | Star         -> Value.binary_star   (eval state e.left) (eval state e.right)
      | Plus         -> Value.binary_plus   (eval state e.left) (eval state e.right)
      (* comparisons *)
      | Greater      -> Value.greater       (eval state e.left) (eval state e.right)
      | GreaterEqual -> Value.greater_equal (eval state e.left) (eval state e.right)
      | Less         -> Value.less          (eval state e.left) (eval state e.right)
      | LessEqual    -> Value.less_equal    (eval state e.left) (eval state e.right)
      (* equality *)
      | BangEqual    -> Value.bang_equal    (eval state e.left) (eval state e.right)
      | EqualEqual   -> Value.equal_equal   (eval state e.left) (eval state e.right)
      | _ -> raise (RuntimeError { where = e.bin_op.line; message = "invalid binary operator" }))
  | Logical e -> (match e.bin_op.token_type with 
      | Or -> let left = eval state e.left in 
        if Value.truthy left then left else eval state e.right
      | And -> let left = eval state e.left in 
        if Value.truthy left then eval state e.right else left
      | _ -> raise (RuntimeError {where = e.bin_op.line; message = "invalid logical operator" })
    )
  | Variable t -> Environment.get state.env t.line t.lexem
  | Assign   e -> 
    let value = eval state e.new_value in 
    Environment.assign state.env e.name.line e.name.lexem value
  | Call e -> 
    let func = eval state e.callee in
    let arguments = List.map (eval state) e.arguments in 

    let expected = Value.arity func in
    let found = List.length arguments in

    if found != expected then begin
      let message = Printf.sprintf "Expected %d arguments but got %d." expected found in 
      let where   = (location expr).line in
      raise (RuntimeError { where; message });
    end;

    Value.call func arguments

let rec interpret state stmnt = 
  try 
    match stmnt with
    | Statement.Expression e -> ignore (eval state e)
    | Print   e -> print_endline (Value.string_of (eval state e))
    | VarDecl r -> let value = match r.initalizer with
        | None -> Value.LoxNil
        | Some e -> eval state e
      in 
      Environment.define state.env r.name.lexem value
    | Block   r -> 
      let previous = state.env in
      let env = Environment.create ?enclosing: (Some previous) () in
      begin try
          state.env <- env;
          interpret_many state r.statements
        with
        | e -> (state.env <- previous ; raise e)
      end;
      state.env <- previous
    | IfStmnt r -> 
      let c = eval state r.condition in 
      if Value.truthy c then
        interpret state r.true_branch
      else 
        (match r.false_branch with
         | None -> ()
         | Some s -> interpret state s;)
    | While r ->
      while  Value.truthy (eval state r.loop_condition) do
        interpret state r.body
      done
    | Function r -> 
      let param_count = List.length r.params in

      (* 
        We can't have dynamic dispatch like the java interface uses, and we
        can't have cyclical module reference, so we can't put the `Statement
        list` or `Block` in a `Value.t`. The work-around here is to take
        advantange of the relation between objects and closures. [Objects are
        just a poor man's closures][1] or whatever.

        [1]: http://wiki.c2.com/?ClosuresAndObjectsAreEquivalent
      *)
      let f args = 
        let env = Environment.create ?enclosing: (Some state.globals) () in

        (* 
          Here's where we capture the enclosing state. For lexical closures
          we'd need to do a bit more, but you get the idea. This would be a
          lot harder wihtout mutation, so thanks Ocaml.
        *) 
        let f_state = { globals = state.globals; env; } in

        (* bind params to args in f_state *)
        List.iter (fun (p, a) -> Environment.define f_state.env p.lexem a) (Utils.zip r.params args);

        let retval = ref Value.LoxNil in

        begin try 
            interpret f_state (Block r.func_body);
          with
          | Return v -> retval := v
        end;
        !retval
      in 
      let value = Value.of_ocaml param_count f in
      Environment.define state.env r.func_name.lexem value
    | Return r -> 
      let retval = match r.value with 
        | None -> Value.LoxNil
        | Some e -> eval state e
      in
      raise (Return retval);
  with
  | Value.TypeError { expected; found } -> 
    let found = Value.string_of_type_name found in
    let expected = Value.string_of_type_name expected in
    let message =  "found '" ^ found  ^ "' expected '" ^ expected ^ "'" in
    raise (RuntimeError { where = (Statement.location stmnt).line; message; })

and interpret_many state = List.iter (interpret state)

let run state src = 
  try 
    let tokens = scan src in 
    let parser = make_parser tokens in
    let statements = Parser.parse parser in
    interpret_many state statements
  with 
  | RuntimeError rte -> runtime_error rte
  | ParseError -> error (-1) "parser error"
