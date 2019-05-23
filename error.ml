
type rte = { where: int; message: string }

exception RuntimeError of rte

let had_error = ref false

(* Report an error, with it's location and source info. *)
let report line where message = 
  Printf.eprintf "[line %d] Error%s: %s\n" line where message;
  flush stderr;
  had_error := true

(* A dumber version of `report` *)
let error line message = report line "" message

let runtime_error { where; message } = 
  Printf.eprintf "[line: %d]: %s\n" where message;
  flush stderr;
  had_error := true