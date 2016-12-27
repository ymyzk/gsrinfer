open OUnit2

open Syntax

let id x = x

let test_string_of_type =
  List.map
    (fun (e, t) -> e >:: fun test_ctxt -> assert_equal ~printer:id e (string_of_type t))
    [
      "'a", TyParam 1;
      "bool", TyBool;
      "int", TyInt;
      "?", TyDyn;
      "int/unit -> bool/int", TyFun (TyInt, TyUnit, TyBool, TyInt);
      "(int/'a -> 'a/'b)/unit -> bool/int",
      TyFun (TyFun (TyInt, TyParam 1, TyParam 1, TyParam 2), TyUnit, TyBool, TyInt);
    ]

    [
      "bool", TyBool, "bool";
      "int", TyInt, "int";
      "?", TyDyn, "?";
      "int/unit -> bool/int", TyFun (TyInt, TyUnit, TyBool, TyInt), "(int/unit -> bool/int)";
(*
      "'a", TyParam 1;
      "bool", TyBool;
      "int", TyInt;
      "?", TyDyn;
      "int/unit -> bool/int", TyFun (TyInt, TyUnit, TyBool, TyInt);
      "(int/'a -> 'a/'b)/unit -> bool/int",
      TyFun (TyFun (TyInt, TyParam 1, TyParam 1, TyParam 2), TyUnit, TyBool, TyInt);
*)
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
