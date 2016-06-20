open Printf
open Syntax

let parse_string s =
  let lexbuf = Lexing.from_string (s ^ ";;") in
  let e = Parser.toplevel Lexer.main lexbuf in
  e

let test1 s =
  let empty = Environment.empty in
  let e = parse_string s in
  let u, c = Typing.generate_constraints empty e in
  print_endline "Constant generation";
  print_endline @@ sprintf "type: %s" @@ Syntax.string_of_type u;
  print_endline @@ sprintf "constraints: %s" @@ Constraints.string_of_constraints c;
  print_endline "Unification";
  let s = Typing.unify c in
  print_endline @@ sprintf "substitutions: %s" @@ Typing.string_of_substitutions s;
  let t = Typing.subst_type_substitutions u s in
  print_endline @@ sprintf "type: %s" @@ Syntax.string_of_type t;
  print_endline "TyVar -> TyParam";
  print_endline @@ sprintf "type: %s" @@ Syntax.string_of_type @@ Typing.type_of_exp empty e
