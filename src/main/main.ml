open Printf
open Typing

let rec read_type_print () =
  print_string "# ";
  flush stdout;
  try
    let empty = Syntax.Environment.empty in
    let e = Parser.toplevel Lexer.main @@ Lexing.from_channel stdin in
    print_endline @@ sprintf "- : %s" @@ Syntax.string_of_type @@ Typing.type_of_exp empty e;
    read_type_print ()
  with
  | Failure message ->
      prerr_endline @@ sprintf "Failure: %s" message;
      read_type_print ()
  | Parser.Error -> (* Menhir *)
      prerr_endline @@ sprintf "Parser.Error";
      read_type_print ()
  | Type_error message ->
      prerr_endline @@ sprintf "Type_error: %s" message;
      read_type_print ()

let () = read_type_print ()
