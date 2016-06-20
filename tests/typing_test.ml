open OUnit2

open Syntax
open Typing

let test_is_static_type =
  List.map
    (fun (l, t, e) -> l >:: fun test_ctxt -> assert_equal (is_static_type t) e)
    [
      "bool", TyBool, true;
      "int", TyInt, true;
      "int -> bool", TyFun (TyInt, TyBool), true;
      "int -> ?", TyFun (TyInt, TyDyn), false;
      "?", TyDyn, false
    ]

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

let suite = [
  "test_is_static_type">::: test_is_static_type;
  "test_tyvars">::: test_tyvars
]
