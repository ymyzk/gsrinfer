open OUnit2

open Syntax

let test_parse =
  let create_case (t, e) =
    t >:: fun test_ctxt ->
      let lexbuf = Lexing.from_string t in
      assert_equal (Parser.toplevel Lexer.main lexbuf) e
  in
  List.map create_case [
    "1;;", Const (ConstInt 1);
    "true;;", Const (ConstBool true);
    "1 + 2;;", BinOp (Plus, Const (ConstInt 1), Const (ConstInt 2));
    "1 + (2 + 3);;",
    BinOp (Plus,
           Const (ConstInt 1),
           BinOp (Plus, Const (ConstInt 2), Const (ConstInt 3)));
    "1 + 2 + 3;;",
    BinOp (Plus,
           BinOp (Plus, Const (ConstInt 1), Const (ConstInt 2)),
           Const (ConstInt 3));
    "reset y;;", Reset (Var "y", None);
    "reset (y);;", Reset (Var "y", None);
    "reset^int (y);;", Reset (Var "y", Some TyInt);
    "x + reset (y);;", BinOp (Plus, Var "x", Reset (Var "y", None));
    "f (reset x);;", App (Var "f", Reset (Var "x", None));
    "shift k -> (k);;", Shift ("k", None, Var "k");
    "shift (k:int/int->bool/int) -> (k);;", Shift ("k", Some (TyFun (TyInt, TyInt, TyBool, TyInt)), Var "k");
    "reset shift k -> shift j -> x;;", Reset (Shift ("k", None, Shift ("j", None, Var "x")), None);
    "fun x -> x;;", Fun (None, "x", None, Var "x");
    "fun (x:int) -> x;;", Fun (None, "x", Some TyInt, Var "x");
    "fun (f:(int/bool->bool/int)) -> f;;",
    Fun (None, "f", Some (TyFun (TyInt, TyBool, TyBool, TyInt)), Var "f");
    "fun^bool x -> x;;", Fun (Some TyBool, "x", None, Var "x");
    "fun^bool (x:int) -> x;;", Fun (Some TyBool, "x", Some TyInt, Var "x");
    "x; y; z;;", Consq (Var "x", Consq (Var "y", Var "z"));
  ]

let suite = [
  "test_parse">::: test_parse
]
