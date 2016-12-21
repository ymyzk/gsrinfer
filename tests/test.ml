open OUnit2

let suite = "test">::: [
  "parser_test">::: Parser_test.suite;
  "syntax_test">::: Syntax_test.suite;
  "typing_test">::: Typing_test.suite;
]

let () = run_test_tt_main suite
