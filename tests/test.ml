open OUnit2

let suite = "test">::: [
  "syntax_test">::: Syntax_test.suite;
  "typing_test">::: Typing_test.suite;
]

let () = run_test_tt_main suite
