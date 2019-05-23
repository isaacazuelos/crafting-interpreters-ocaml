(* 
  This is an attempt to port the tree walking interpreter from [_Crafting
  Interpreters_][ci] to OCaml.

  [ci]: http://craftinginterpreters.com/
*)

open Utils
open Interpreter
open Error

let usage = "Usage: jlox [script]"

let run_prompt () = 
  let state = Interpreter.create () in 
  while true do
    print_string ">>> ";
    flush stdout;
    let line = input_line stdin in 
    run state line;
    had_error := false
  done

let run_file file_name = 
  let contents = read_file file_name in 
  let state = Interpreter.create () in
  run state contents;
  if !had_error then exit 65

let () = 
  let arg_length = Array.length Sys.argv in
  if arg_length > 2 then begin
    print_endline usage;
    exit 64
  end else if arg_length = 2 then
    let file_name = Array.get Sys.argv 1 in 
    run_file file_name
  else 
    run_prompt ()   
