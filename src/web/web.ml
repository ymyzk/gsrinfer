let _ =
  Js.export "GsrInfer" @@
    object%js
      method infer x = begin
        let empty = Syntax.Environment.empty in
        try
          let x = Js.to_string x in
          let e = Parser.toplevel Lexer.main @@ Lexing.from_string x in
          let s = Printf.sprintf "- : %s" @@ Syntax.string_of_type @@ Typing.type_of_exp empty e in
          object%js
            val isSucceeded = Js._true
            val result = Js.string s
          end
        with
        | Failure message ->
            object%js
              val isSucceeded = Js._false
              val result = Js.string @@ Printf.sprintf "Failure: %s" message
            end
        | Parser.Error -> (* Menhir *)
            object%js
              val isSucceeded = Js._false
              val result = Js.string "Parser.Error"
            end
        | Typing.Type_error message ->
            object%js
              val isSucceeded = Js._false
              val result = Js.string @@ Printf.sprintf "Type_error: %s" message
            end
        | _ ->
            object%js
              val isSucceeded = Js._false
              val result = Js.string "Unexpected error"
            end
      end
    end
