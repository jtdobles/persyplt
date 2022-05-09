(* To do: edit this code to make it work with our parser, scanner & semant *)
open Ast
(*open Sast*)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Dparse.program Scanner.tokenize lexbuf in
  let sprogram = Semant.check program in
  print_endline (string_of_program sprogram)