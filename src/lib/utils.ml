open Syntax

let parse_string s =
  let lexbuf = Lexing.from_string (s ^ ";;") in
  let e = Parser.toplevel Lexer.main lexbuf in
  e

let test1 s =
  let empty = Environment.empty in
  let e = parse_string s in
  let u, c = Typing.generate_constraints empty e in
  print_endline @@ Syntax.string_of_type u;
  print_endline @@ Typing.string_of_constraints c;
  Typing.unify c
