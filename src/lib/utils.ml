open Core.Std

open Printf
open Syntax

let parse_string s =
  let lexbuf = Lexing.from_string (s ^ ";;") in
  let e = Parser.toplevel Lexer.main lexbuf in
  e

(*
let test1 s =
  let empty = String.Map.empty in
  let e = parse_string s in
  let u, a, b, c = Typing.generate_constraints empty e in
  print_endline "Constrait generation";
  print_endline @@ sprintf "C: %s" @@ Constraints.string_of_constraints c;
  print_endline @@ sprintf "T: %s" @@ Syntax.string_of_type u;
  print_endline @@ sprintf "α: %s" @@ Syntax.string_of_type a;
  print_endline @@ sprintf "β: %s" @@ Syntax.string_of_type b;
  print_endline "";
  print_endline "Unification";
  let s = Typing.unify c in
  print_endline @@ sprintf "S: %s" @@ Typing.string_of_substitutions s;
  let t = Typing.subst_type_substitutions u s in
  print_endline @@ sprintf "T: %s" @@ Syntax.string_of_type t;
  print_endline @@ sprintf "α: %s" @@ Syntax.string_of_type a;
  print_endline @@ sprintf "β: %s" @@ Syntax.string_of_type b;
  print_endline "";
  print_endline "TyVar -> TyParam";
  print_endline @@ sprintf "T: %s" @@ Syntax.string_of_type @@ Typing.type_of_exp empty e;
(*
  print_endline @@ sprintf "α: %s" @@ Syntax.string_of_type @@ Typing.type_of_exp empty a;
  print_endline @@ sprintf "β: %s" @@ Syntax.string_of_type @@ Typing.type_of_exp empty b;
*)
  print_endline ""
*)
