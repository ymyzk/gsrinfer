module Environment = Map.Make (
  struct
    type t = Syntax.id
    let compare = compare
  end
)
(*

let string_of_environment env =
  if Environment.is_empty env then
    "."
  else
    let bindings = Environment.bindings env in
    String.concat ", " @@
      List.map (fun (id, ty) -> id ^ ":" ^ string_of_type ty) bindings
*)
