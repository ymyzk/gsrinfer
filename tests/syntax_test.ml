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

let test_string_of_exp =
  List.map
    (fun (e, t) -> e >:: fun test_ctxt -> assert_equal ~printer:id e (string_of_exp t))
    [
      "x", Var "x";
      "1", Const (ConstInt 1);
      "true", Const (ConstBool true);
      "()", Const ConstUnit;
      "x + y", BinOp (Plus, Var "x", Var "y");
    ]

let suite = [
  "test_string_of_type">::: test_string_of_type;
  "test_string_of_exp">::: test_string_of_exp;
]
