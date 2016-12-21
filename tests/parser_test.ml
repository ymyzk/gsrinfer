open OUnit2

open Syntax

let test_parse =
  let create_case (t, e) =
    t >:: fun test_ctxt ->
      let lexbuf = Lexing.from_string t in
      assert_equal ~printer:string_of_exp (Parser.toplevel Lexer.main lexbuf) e
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
    "reset (fun () -> y);;", Reset (Var "y", None);
    "x + reset (fun () -> y);;", BinOp (Plus, Var "x", Reset (Var "y", None));
    "f reset (fun () -> x);;", App (Var "f", Reset (Var "x", None));
    "fun x -> x;;", Fun (None, "x", None, Var "x");
    "fun (x:int) -> x;;", Fun (None, "x", Some TyInt, Var "x");
    "fun (f:(int/bool->bool/int)) -> f;;",
    Fun (None, "f", Some (TyFun (TyInt, TyBool, TyBool, TyInt)), Var "f");
  ]

let suite = [
  "test_parse">::: test_parse
]
