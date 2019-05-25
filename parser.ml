open Scanner
open Expression
open Statement

type parser = {
  tokens: token array;
  mutable current: int;
}

let make_parser tokens = { tokens = Array.of_list tokens; current = 0; }
let at_end parser = parser.current >= Array.length parser.tokens - 1
let previous parser = Array.get parser.tokens (parser.current - 1)
let peek parser = Array.get parser.tokens parser.current

let advance parser = 
  if not (at_end parser) then 
    parser.current <- parser.current + 1;
  previous parser

let advance_ parser = ignore (advance parser)

let check parser token_type =
  if at_end parser then 
    false
  else
    (peek parser).token_type == token_type

let rec matches parser token_types = 
  match token_types with
  | [] -> false
  | t::ts -> if check parser t then begin
      advance_ parser; 
      true 
    end else 
      matches parser ts

exception ParseError
exception Break

let error token message = 
  (* We can't override Error.error, so we inline that definition here *)
  if token.token_type = Eof then
    Error.report token.line " at end" message
  else 
    Error.report token.line (" at '" ^ token.lexem ^ "'") message;
  ParseError

let consume parser token_type message = 
  if check parser token_type then 
    advance parser 
  else 
    raise (error (peek parser) message)

let consume_ parser token_type message = 
  ignore (consume parser token_type message)

let synchronize parser = 
  advance_ parser;

  try 
    while not (at_end parser) do
      if (previous parser).token_type = Semicolon then 
        raise Break
      else 
        match (peek parser).token_type with
        | Class | Fun | Var | For | If | While | Print | Return -> raise Break
        | _ -> advance_ parser;
    done
  with
  | Break -> ()

let rec binary next_precedence token_types parser = 
  let expr = ref (next_precedence parser) in
  while matches parser token_types do
    let bin_op = previous parser in
    let right = next_precedence parser in
    expr := Binary { left = !expr; right; bin_op }
  done;
  !expr

and logical next_precedence token_type parser = 
  let expr = ref (next_precedence parser) in
  while matches parser [token_type] do
    let bin_op = previous parser in
    let right = next_precedence parser in
    expr := Logical { left = !expr; right; bin_op }
  done;
  !expr

and unary parser = 
  if matches parser [Bang; Minus] then 
    let unary_op = previous parser in
    (* We're recursing here, maybe that's not intended? *)
    let opperand = unary parser in
    Unary { unary_op; opperand }
  else
    call parser

and call parser = 
  let expr = ref (primary parser) in

  let continue = ref true in
  while !continue do 

    if matches parser [LeftParen] then
      expr := finishCall parser !expr
    else
      continue := false
  done;

  !expr

and finishCall parser callee = 
  let arguments = ref [] in
  let count = ref 0 in

  if not (check parser RightParen) then begin
    arguments := expression parser :: !arguments;
    count := !count + 1;
    while matches parser [Comma] do 
      if !count >= 8 then
        (* We intentionally don't throw to match the Java version, but that's
           maybe not the best call here. *) 
        ignore (error (peek parser) "Cannot have more than 8 arguments.");

      count := !count + 1;
      arguments := expression parser :: !arguments;
    done;
  end;

  let paren = consume parser RightParen "Expect ')' after arguments." in
  Call { callee; paren; arguments = !arguments }

and primary parser =
  let token = advance parser in
  match token.token_type with
  | False      -> Literal { token; value = LoxBool false }
  | True       -> Literal { token; value = LoxBool true }
  | Nil        -> Literal { token; value = LoxNil }
  | Number     -> Literal { token; value = LoxNumber (float_of_string token.lexem) }
  | String     -> Literal { token; value = LoxString (token.lexem) }
  | Identifier -> Variable token
  | LeftParen  ->
    let expr = expression parser in
    consume_ parser RightParen "Expect ')' after expression.";
    Grouping { expression = expr }
  | _ -> raise (error token "Expect expression.")

and multiplication parser = binary unary [Slash; Star] parser
and addition parser = binary multiplication [Minus; Plus] parser
and comparison parser = binary addition [Greater; GreaterEqual; Less; LessEqual] parser
and equality parser = binary comparison [BangEqual; EqualEqual] parser

and expression parser = assignment parser

and or_expr parser = logical and_expr Or parser
and and_expr parser = logical equality And parser

and assignment parser = 
  let expr = or_expr parser in

  if matches parser [Equal] then
    let equals = previous parser in
    let new_value  = assignment parser in

    match expr with
    | Variable name -> Assign { name; new_value }
    | _ -> raise (error equals "Invalid assingment target.")

  else
    expr

and print_statement parser = 
  let e = expression parser in 
  consume_ parser Semicolon "Expect ';' after value.";
  Print e

and return_statment parser =
  let keyword = advance parser in
  let value = if check parser Semicolon then
      None
    else 
      Some (expression parser) 
  in
  consume_ parser Semicolon "Expected ';' after return value.";
  Statement.Return { keyword; value }

and expression_statement parser = 
  let e = expression parser in 
  consume_ parser Semicolon "Expect ';' after expression.";
  Expression e

and var_declaration parser = 
  let name = consume parser Identifier "Expect variable name." in
  let initalizer = if matches parser [Equal] then Some (expression parser) else None in
  consume_ parser Semicolon "Expect ';' after variable declaration.";
  VarDecl { name; initalizer }  

(* renamed to avoid `funciton` keyword in Ocaml. *)
and function_declaration parser kind = 
  let func_name = consume parser Identifier ("Expect " ^ kind ^ " name.") in
  consume_ parser LeftParen ("Expect '(' after " ^ kind ^ " name.");

  let parameters = ref [] in
  let count = ref 0 in

  if not (check parser RightParen) then begin

    parameters := (consume parser Identifier "Expect parameter name.") :: !parameters;
    count := !count + 1;

    while matches parser [Comma] do
      if !count >= 8 then
        Error.error (peek parser).line "Cannot have more than 8 parameters.";

      parameters := (consume parser Identifier "Expect parameter name.") :: !parameters;
      count := !count + 1;
    done;
  end;
  let params = List.rev !parameters in

  consume_ parser RightParen "Expect ')' after parameters name.";

  let func_body = block parser in
  Function { func_name; params; func_body }

and block parser = 
  let open_brace = consume parser LeftBrace ("Expect '{' before block.") in
  let statements = ref [] in

  while not (at_end parser) && not (check parser RightBrace) do
    match declaration parser with 
    | Some s -> statements := s :: !statements
    | None   -> ()
  done;

  consume_ parser RightBrace "Expect '}' after block.";
  let statements = List.rev !statements in
  { statements; open_brace }

and if_stmnt parser =
  consume_ parser LeftParen "Expect '(' after an 'if'.";
  let condition = expression parser in
  consume_ parser RightParen "Expect ')' after an if condition.";

  let true_branch = statement parser in
  let false_branch = if matches parser [Else] then
      Some (statement parser)
    else 
      None 
  in

  IfStmnt { condition; true_branch; false_branch }

and while_stmnt parser =
  consume_ parser LeftParen "Expect '(' after an 'if'.";
  let loop_condition = expression parser in
  consume_ parser RightParen "Expect ')' after an if condition.";
  let body = statement parser in
  While { body; loop_condition }

and for_stmnt parser = 
  (* This may be a bit much *)
  consume_ parser LeftParen "Expect '(' after 'for'.";
  let initalizer = if matches parser [Semicolon] then
      None
    else if matches parser [Var] then
      Some (var_declaration parser)
    else 
      Some (expression_statement parser)
  in

  let condition = if not (check parser Semicolon) then
      Some (expression parser)
    else 
      None
  in 
  advance_ parser;

  let increment = if not (check parser Semicolon) then
      Some (expression parser)
    else 
      None
  in 

  consume_ parser RightParen "Expect ')' after for clauses.";
  let body = ref (statement parser) in

  (* body = { body; incrment }; *)
  begin match increment with
    | Some inc -> body := Block { open_brace = location !body; statements = [ !body; Expression inc ]; };
    | None -> ()
  end;

  (* body = while (condition) {body ; increment } *)
  begin match condition with
    | Some loop_condition -> body := While { loop_condition; body = !body };
    | None -> ()
  end;

  (* body = { initalizer; while (condition) {body ; increment } } *)
  begin match initalizer with
    | Some init -> body := Block { open_brace = location init; statements = [ init; !body ] };
    | None -> ()
  end;

  !body

and declaration parser: stmnt option =
  try 
    match (peek parser).token_type with
    | Var -> advance_ parser; Some (var_declaration parser)
    | Fun -> advance_ parser; Some (function_declaration parser "function")
    | _   -> Some (statement parser)
  with 
  | ParseError -> 
    synchronize parser; None 

and statement parser =
  match (peek parser).token_type with
  | Print     -> advance_ parser; print_statement parser
  | Return    -> return_statment parser
  | LeftBrace -> Block (block parser)
  | If        -> advance_ parser; if_stmnt parser
  | While     -> advance_ parser; while_stmnt parser
  | For       -> advance_ parser; for_stmnt parser
  | _ -> expression_statement parser

and parse parser =
  let statements = ref [] in
  while not (at_end parser) do
    match declaration parser with
    | None   -> ()
    | Some s -> statements := s :: !statements;
  done;
  List.rev !statements
