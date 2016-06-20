open OUnit2

open Syntax

let test_string_of_type =
  List.map
    (fun (l, t, e) -> l >:: fun test_ctxt -> assert_equal (string_of_type t) e)
    [
      "bool", TyBool, "bool";
      "int", TyInt, "int";
      "?", TyDyn, "?";
      "int -> bool", TyFun (TyInt, TyBool), "int -> bool";
      "int -> ?", TyFun (TyInt, TyDyn), "int -> ?";
      "'a -> 'b", TyFun (TyParam 1, TyParam 0), "'a -> 'b";
      "'a -> 'b", TyFun (TyFun (TyParam 1, TyParam 0), TyParam 2), "('a -> 'b) -> 'c";
    ]

(*
let test_tyvars =
  List.map
    (fun (l, t, e) -> l >:: fun test_ctxt -> assert_equal (tyvars t) e)
    [
      "int", TyInt, Variables.empty;
      "?", TyDyn, Variables.empty;
      "$0", TyVar 0, Variables.singleton 0;
      "int -> $0", TyFun (TyInt, TyVar 0), Variables.singleton 0;
      "$0 -> $0", TyFun (TyVar 0, TyVar 0), Variables.singleton 0;
      "$0 -> $1", TyFun (TyVar 0, TyVar 1), Variables.add 1 @@ Variables.singleton 0
    ]
*)

let suite = [
  "test_string_of_type">::: test_string_of_type
]
