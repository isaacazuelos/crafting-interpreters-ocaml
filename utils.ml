(* 
  Utility functions, for when we need to reinvent the wheel because of OCaml's
  minimal standard libraries.
*)

let read_file file_name = 
  let file = open_in file_name in
  let rec go acc = 
    try
      let line = input_line file in 
      go (line :: acc)
    with End_of_file ->
      close_in file;
      acc
  in
  let lines = go [] in 
  (* we need to put the newlines back in, since `input_line` removes them. *)
  String.concat "\n" (List.rev lines) 

let rec zip xs ys = match (xs, ys) with
  | ([], []) -> []
  | (x::xs, y::ys) -> (x, y) :: zip xs ys
  | _ -> raise (Invalid_argument "zip needs lists of the same length")