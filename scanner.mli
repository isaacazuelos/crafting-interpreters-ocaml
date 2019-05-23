type token_type = 
  (* Single-Character Tokens. *)
  | LeftParen | RightParen | LeftBrace | RightBrace | Comma | Dot | Minus 
  | Plus | Semicolon | Slash | Star
  (* One or two character tokens. *)
  | Bang | BangEqual | Equal | EqualEqual | Greater | GreaterEqual | Less 
  | LessEqual
  (* Literals. *)
  | Identifier | String | Number
  (* Keywords. *)
  | And | Class | Else | False | Fun | For | If | Nil | Or | Print | Return
  | Super | This | True | Var | While | Eof

type token = { 
  token_type: token_type;
  lexem: string;
  literal: Value.t option;
  line: int;
}

val string_of : token -> string
val scan : string -> token list
val lexem : token -> string