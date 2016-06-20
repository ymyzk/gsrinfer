open OUnit2

let suite = "test">::: [
  "typing_test">::: Typing_test.suite
]

let () = run_test_tt_main suite
