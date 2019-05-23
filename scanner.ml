open Error

type token_type = 
  (* Single-Character Tokens. *)
  | LeftParen | RightParen | LeftBrace | RightBrace 
  | Comma | Dot | Minus | Plus | Semicolon | Slash | Star
  (* One or two character tokens. *)
  | Bang | BangEqual | Equal | EqualEqual 
  | Greater | GreaterEqual | Less | LessEqual
  (* Literals. *)
  | Identifier | String | Number
  (* Keywords. *)
  | And | Class | Else | False | Fun | For | If | Nil | Or | Print | Return 
  | Super | This | True | Var | While | Eof

let string_of_token_type tt = match tt with 
  (* Single-Character Tokens. *)
  | LeftParen -> "("
  | RightParen -> ")"
  | LeftBrace -> "{"
  | RightBrace -> "}"
  | Comma -> ","
  | Dot -> "."
  | Minus -> "-"
  | Plus -> "+"
  | Semicolon -> ";"
  | Slash -> "/"
  | Star -> "*"
  (* One or two character tokens. *)
  | Bang -> "!"
  | BangEqual -> "!="
  | Equal -> "="
  | EqualEqual -> "=="
  | Greater -> ">"
  | GreaterEqual -> ">="
  | Less -> ">"
  | LessEqual -> "<=" 
  (* Literals. *)
  | Identifier -> "<IDENT>"
  | String -> "<STRING>"
  | Number -> "<NUMBER>"
  (* Keywords. *)
  | And -> "and"
  | Class -> "class"
  | Else -> "else"
  | False -> "false"
  | Fun -> "fun"
  | For -> "for"
  | If -> "if"
  | Nil -> "nil"
  | Or -> "or"
  | Print -> "print"
  | Return -> "return"
  | Super -> "super"
  | This -> "this"
  | True -> "true"
  | Var -> "var"
  | While -> "while"
  | Eof -> "EOF"


(* Just going wiht an a-list here for simplicity's sake *)
let keywords = 
  [ ("and", And)
  ; ("class", Class)
  ; ("else", Else)
  ; ("false", False)
  ; ("fun", Fun)
  ; ("for", For)
  ; ("if", If)
  ; ("nil", Nil)
  ; ("or", Or)
  ; ("print", Print)
  ; ("return", Return)
  ; ("super", Super)
  ; ("this", This)
  ; ("true", True)
  ; ("var", Var)
  ; ("while", While)
  ]

type token = { 
  token_type: token_type;
  lexem: string;
  literal: Value.t option;
  line: int;
}

let lexem tok = tok.lexem

let string_of tok = 
  let tt = string_of_token_type tok.token_type in
  match tok.token_type with
  | Number | String | Identifier -> Printf.sprintf "%s %s" tt tok.lexem
  | _ -> Printf.sprintf "%s" tt

(* String.concat " " [ string_of_token_type tok.token_type
                  ; tok.lexem
                  ; match tok.literal with
                  | None -> ""
                  | Some s -> string_of_literal s
                  ] *)

type scanner =  {
  source: string;
  mutable tokens: token list;
  mutable start: int;
  mutable current: int;
  mutable line: int;
}

let make_scanner source = { 
  source;
  tokens = [];
  start = 0;
  current = 0;
  line = 1;
}

let at_end scanner = scanner.current >= String.length scanner.source 

let add_token scanner ?literal token_type =
  let length = scanner.current - scanner.start in
  let lexem = String.sub scanner.source scanner.start length in 
  let new_token = { token_type; lexem; literal; line = scanner.line; } in 
  scanner.tokens <- new_token :: scanner.tokens

let peek scanner = 
  let cursor = scanner.current in
  String.get scanner.source cursor

let advance scanner = 
  let cursor = scanner.current in
  scanner.current <- cursor + 1;
  String.get scanner.source cursor

(* A version of `advance` that returns `unit` *)
let advance_ scanner = ignore (advance scanner)

let matches scanner expected =
  if at_end scanner then 
    false 
  else 
    let cursor = peek scanner in
    if cursor != expected then
      false
    else begin 
      scanner.current <- scanner.current + 1;
      true
    end

let string scanner = 
  while not (at_end scanner) && peek scanner != '"' do
    if peek scanner = '\n' then scanner.line <- scanner.line + 1;
    advance_ scanner;
  done;

  if at_end scanner then
    error scanner.line "Unterminated string."
  else begin
    advance_ scanner;
    let length = scanner.current - scanner.start in
    let value = Value.LoxString (String.sub scanner.source (scanner.start + 1) (length - 1)) in
    (* Here's where we'd process escapes *)
    add_token scanner ?literal: (Some value) String
  end

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'z') || c == '_'
let is_alphanumeric c = is_digit c || is_alpha c

let peek_next scanner = 
  if scanner.current + 1 >= String.length scanner.source then
    char_of_int 0
  else 
    String.get scanner.source (scanner.current + 1)

let number scanner = 
  while not (at_end scanner) && is_digit (peek scanner) do 
    advance_ scanner 
  done;

  (* look for a fractional part *)
  if not (at_end scanner) 
  && peek scanner = '.'
  && is_digit (peek_next scanner) then begin
    (* consume the '.' *)
    advance_ scanner;

    while not (at_end scanner) && is_digit (peek scanner) do 
      advance_ scanner 
    done;
  end;

  let length = scanner.current - scanner.start in
  let number_chars = String.sub scanner.source (scanner.start) length in 
  let value = Value.LoxNumber (float_of_string (number_chars)) in
  add_token scanner ?literal: (Some value) Number

let identifier scanner = 
  while not (at_end scanner) && is_alphanumeric (peek scanner) do 
    advance_ scanner 
  done;

  let length = scanner.current - scanner.start in
  let text = String.sub scanner.source scanner.start length in
  let token_type = match List.assoc_opt text keywords with
    | None -> Identifier
    | Some t -> t
  in
  add_token scanner token_type

let scan_token scanner =
  let c = advance scanner in 
  match c with 
  (* single character tokens *)
  | '(' -> add_token scanner LeftParen
  | ')' -> add_token scanner RightParen
  | '{' -> add_token scanner LeftBrace
  | '}' -> add_token scanner RightBrace
  | ',' -> add_token scanner Comma
  | '.' -> add_token scanner Dot
  | '-' -> add_token scanner Minus
  | '+' -> add_token scanner Plus
  | ';' -> add_token scanner Semicolon
  | '*' -> add_token scanner Star
  (* operators *)
  | '!' -> add_token scanner (if matches scanner '=' then BangEqual else Bang)
  | '=' -> add_token scanner (if matches scanner '=' then EqualEqual else Equal)
  | '<' -> add_token scanner (if matches scanner '=' then LessEqual else Less)
  | '>' -> add_token scanner (if matches scanner '=' then GreaterEqual else Greater)
  (* longer lexemes *)
  | '/' -> if matches scanner '/' then 
      (* The order here is swapped to fix that `input_line` stole our newline. *) 
      while not (at_end scanner) && peek scanner != '\n' do 
        advance_ scanner
      done
    else 
      add_token scanner Slash
  | ' ' | '\r' | '\t' -> ()
  | '\n' -> scanner.line <- scanner.line + 1
  | '"' -> string scanner
  (* default *)
  | c when is_digit c -> number scanner
  | c when is_alpha c -> identifier scanner
  | c -> error scanner.line ("Unexpected character: " ^ String.make 1 c)

let scan_tokens scanner = 
  while not (at_end scanner) do
    scanner.start <- scanner.current;
    scan_token scanner
  done;
  let eof_token = {
    token_type = Eof;
    lexem = "";
    literal = None;
    line = scanner.line;
  } in
  scanner.tokens <- eof_token :: scanner.tokens;
  scanner.tokens

let scan src = List.rev (scan_tokens (make_scanner src))