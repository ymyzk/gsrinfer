open Syntax

type constr =
  | ConstrEqual of ty * ty
  | ConstrConsistent of ty * ty

module Constraints = Set.Make(
  struct
    type t = constr
    let compare (x : constr) y = compare x y
  end
)

let map_constraints f c =
  Constraints.fold (fun x l -> (f x) :: l) c []

let string_of_constr = function
  | ConstrEqual (u1, u2) -> (string_of_type u1) ^ "=" ^ (string_of_type u2)
  | ConstrConsistent (u1, u2) -> (string_of_type u1) ^ "~" ^ (string_of_type u2)

let string_of_constraints c =
  String.concat ", " @@ map_constraints string_of_constr c

(* [x:=t]u *)
let rec subst_type x t u = match u with
  | TyFun (u1, u2) -> TyFun (subst_type x t u1, subst_type x t u2)
  | TyVar x' -> if x = x' then t else u
  | _ -> u

let rec subst_type_constraint x t = function
  | ConstrEqual (u1, u2) -> ConstrEqual (subst_type x t u1, subst_type x t u2)
  | ConstrConsistent (u1, u2) -> ConstrConsistent (subst_type x t u1, subst_type x t u2)

let rec subst_type_constraints x t c =
  (* TODO: OK? *)
  List.map (subst_type_constraint x t) c
