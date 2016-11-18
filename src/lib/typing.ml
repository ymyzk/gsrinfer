open Core.Std

open Constraints
open Syntax

exception Type_error of string

(* Utilities *)

(* ty -> bool *)
let rec is_static_type = function
  | TyFun (t1, t2, t3, t4) -> (is_static_type t1) && (is_static_type t2) && (is_static_type t3) && (is_static_type t4)
  | TyDyn -> false
  | _ -> true

(* ty list -> bool *)
let is_static_types types = List.fold_left ~init:true ~f:(&&) @@ List.map ~f:is_static_type types

let is_base_type = function
  | TyInt | TyBool | TyUnit -> true
  | _ -> false

let is_tyvar = function
  | TyVar _ -> true
  | _ -> false

let is_typaram = function
  | TyParam _ -> true
  | _ -> false

(* Base type, type variables, or type parameters *)
let is_bvp_type t = is_base_type t || is_tyvar t || is_typaram t

(* Type Variables *)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    TyVar (v + 1)
  in body

let rec tyvars = function
  | TyVar x -> Int.Set.singleton x
  | TyFun (t1, t2, t3, t4) ->
      let vars = Int.Set.union (tyvars t1) (tyvars t2) in
      let vars = Int.Set.union vars (tyvars t3) in
      let vars = Int.Set.union vars (tyvars t4) in
      vars
  | _ -> Int.Set.empty

(* Substitutions *)

type substitution = tyvar * ty
type substitutions = substitution list

let string_of_substitution (x, t) =
  Printf.sprintf "x%d=%s" x @@ string_of_type t

let string_of_substitutions s =
  String.concat ~sep:", " @@ List.map s string_of_substitution

let subst_type_substitutions (t : ty) (s : substitutions) =
  List.fold_left s ~init:t ~f:(fun u -> fun (x, t) -> subst_type x t u)

let fresh_typaram =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    TyParam (v + 1)
  in body

let rec subst_tyvar t m = match t with
  | TyVar x -> Int.Map.find_exn m x
  | TyFun (t1, t2, t3, t4) -> TyFun (subst_tyvar t1 m, subst_tyvar t2 m, subst_tyvar t3 m, subst_tyvar t4 m)
  | _ -> t

(* Create map from type parameters to type variables *)
let create_tyvar_typaram_map t =
  Int.Set.fold (tyvars t) ~init:Int.Map.empty ~f:(fun m x ->
    if Int.Map.mem m x then
      m
    else
      Int.Map.add m x (fresh_typaram ())
  )

(* Replace type variables with type parameters *)
let subst_tyvars (t : ty) : ty =
  subst_tyvar t @@ create_tyvar_typaram_map t

(* Type Inference *)

(* domf = *)
let generate_constraints_domf_eq = function
| TyVar x ->
    let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    x1, Constraints.singleton @@ ConstrEqual ((TyVar x), (TyFun (x1, x2, x3, x4)))
| TyFun (u1, u2, u3, u4) -> u1, Constraints.empty
| TyDyn -> TyDyn, Constraints.empty
| _ -> raise @@ Type_error "error"

(* domc = *)
let generate_constraints_domc_eq = function
| TyVar x ->
    let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    x3, Constraints.singleton @@ ConstrEqual ((TyVar x), (TyFun (x1, x2, x3, x4)))
| TyFun (u1, u2, u3, u4) -> u3, Constraints.empty
| TyDyn -> TyDyn, Constraints.empty
| _ -> raise @@ Type_error "error"

(* codc = *)
let generate_constraints_codc_eq = function
| TyVar x ->
    let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    x2, Constraints.singleton @@ ConstrEqual ((TyVar x), (TyFun (x1, x2, x3, x4)))
| TyFun (u1, u2, u3, u4) -> u2, Constraints.empty
| TyDyn -> TyDyn, Constraints.empty
| _ -> raise @@ Type_error "error"

(* codf = *)
let generate_constraints_codf_eq = function
| TyVar x ->
    let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    x4, Constraints.singleton @@ ConstrEqual ((TyVar x), (TyFun (x1, x2, x3, x4)))
| TyFun (u1, u2, u3, u4) -> u4, Constraints.empty
| TyDyn -> TyDyn, Constraints.empty
| _ -> raise @@ Type_error "error"

(* domf ~ *)
let generate_constraints_domf_con u1 u2 = match u1 with
| TyVar x ->
    let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    let c = Constraints.singleton @@ ConstrEqual ((TyVar x), (TyFun (x1, x2, x3, x4))) in
    Constraints.add (ConstrConsistent (x1, u2)) c
| TyFun (u11, u12, u13, u14) ->
    Constraints.singleton @@ ConstrConsistent (u11, u2)
| TyDyn -> Constraints.singleton @@ ConstrConsistent (u1, u2)
| _ -> raise @@ Type_error "error"

(* codf ~ *)
let generate_constraints_codf_con u1 u2 = match u1 with
| TyVar x ->
    let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
    let c = Constraints.singleton @@ ConstrEqual ((TyVar x), (TyFun (x1, x2, x3, x4))) in
    Constraints.add (ConstrConsistent (x4, u2)) c
| TyFun (u11, u12, u13, u14) ->
    Constraints.singleton @@ ConstrConsistent (u14, u2)
| TyDyn -> Constraints.singleton @@ ConstrConsistent (u1, u2)
| _ -> raise @@ Type_error "error"

let rec generate_constraints_join u1 u2 = match u1, u2 with
| TyInt, TyInt -> TyInt, Constraints.empty
| TyBool, TyBool -> TyBool, Constraints.empty
| TyUnit, TyUnit -> TyUnit, Constraints.empty
| _, TyDyn -> u1, Constraints.singleton @@ ConstrConsistent (u1, TyDyn)
| TyDyn, _ -> u2, Constraints.singleton @@ ConstrConsistent (TyDyn, u2)
| TyVar _, _ -> u1, Constraints.singleton @@ ConstrConsistent (u1, u2)
| _, TyVar _ -> u2, Constraints.singleton @@ ConstrConsistent (u1, u2)
| TyFun (u11, u12, u13, u14), TyFun (u21, u22, u23, u24) ->
    let u1, c1 = generate_constraints_join u11 u21 in
    let u2, c2 = generate_constraints_join u12 u22 in
    let u3, c3 = generate_constraints_join u13 u23 in
    let u4, c4 = generate_constraints_join u14 u24 in
    let c = Constraints.union c1 c2 in
    let c = Constraints.union c c3 in
    let c = Constraints.union c c4 in
    TyFun (u1, u2, u3, u4), c
| _ -> raise @@ Type_error "error: generate_constraints_join"

let generate_constraints env e =
  let rec generate_constraints env e =
    let t, a, b, c = match e with
      | Var x -> begin
          match String.Map.find env x with
          | Some t ->
              let x = fresh_tyvar () in
              t, x, x, Constraints.empty
          | None ->
              raise @@ Type_error (Printf.sprintf "variable '%s' not found in the environment" x)
        end
      | Const c ->
          let t = begin match c with
          | ConstBool b -> TyBool
          | ConstInt i -> TyInt
          | ConstUnit -> TyUnit
          end in
          let x = fresh_tyvar () in
          t, x, x, Constraints.empty
      | BinOp (op, e1, e2) ->
          let u1, b1, g, c1 = generate_constraints env e1 in
          let u2, a, b2, c2 = generate_constraints env e2 in
          let c = Constraints.union c1 c2 in
          let c = Constraints.add (ConstrEqual (b1, b2)) c in
          let c = Constraints.add (ConstrConsistent (u1, TyInt)) c in
          let c = Constraints.add (ConstrConsistent (u2, TyInt)) c in
          TyInt, a, g, c
      | FunI (x, e) ->
          let x_a, x_t = fresh_tyvar (), fresh_tyvar () in
          let u, b, g, c = generate_constraints (String.Map.add env x x_t) e in
          TyFun (x_t, b, u, g), x_a, x_a, c
      | FunE (x, x_t, e) ->
          let x_a = fresh_tyvar () in
          let u, b, g, c = generate_constraints (String.Map.add env x x_t) e in
          TyFun (x_t, b, u, g), x_a, x_a, c
      | App (e1, e2) ->
          let u1, g, d, c1 = generate_constraints env e1 in
          let u2, b, g', c2 = generate_constraints env e2 in
          let u, c3 = generate_constraints_domc_eq u1 in
          let a, c4 = generate_constraints_codc_eq u1 in
          let c5 = generate_constraints_codf_con u1 b in
          let c6 = generate_constraints_domf_con u1 u2 in
          let c = Constraints.union c1 c2 in
          let c = Constraints.union c c3 in
          let c = Constraints.union c c4 in
          let c = Constraints.union c c5 in
          let c = Constraints.union c c6 in
          let c = Constraints.add (ConstrEqual (g, g')) c in
          u, a, d, c
      | ShiftI (k, e) ->
          let x, x_a, x_g = fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
          let env' = String.Map.add env k (TyFun (x, x_g, x_a, x_g)) in
          let d, d', b, c = generate_constraints env' e in
          let c = Constraints.add (ConstrConsistent (d, d')) c in
          x, x_a, b, c
      | ShiftE (k, s, e) ->
          let d, d', b, c1 = generate_constraints (String.Map.add env k s) e in
          let a, c2 = generate_constraints_domc_eq s in
          let u, c3 = generate_constraints_domf_eq s in
          let g1, c4 = generate_constraints_codc_eq s in
          let g2, c5 = generate_constraints_codf_eq s in
          let _, c6 = generate_constraints_join g1 g2 in
          let c = Constraints.union c1 c2 in
          let c = Constraints.union c c3 in
          let c = Constraints.union c c4 in
          let c = Constraints.union c c5 in
          let c = Constraints.union c c6 in
          let c = Constraints.add (ConstrEqual (d, d')) c in
          u, a, b, c
      | ResetI e ->
          let x = fresh_tyvar () in
          let b, b', t, c = generate_constraints env e in
          let c = Constraints.add (ConstrConsistent (b, b')) c in
          t, x, x, c
      | If (e0, e1, e2) ->
          let t0, d0, b, c0 = generate_constraints env e0 in
          let t1, a1, d1, c1 = generate_constraints env e1 in
          let t2, a2, d2, c2 = generate_constraints env e2 in
          let a, c3 = generate_constraints_join a1 a2 in
          let t, c4 = generate_constraints_join t1 t2 in
          let c = Constraints.union c1 c2 in
          let c = Constraints.union c c3 in
          let c = Constraints.union c c4 in
          let c = Constraints.add (ConstrConsistent (t0, TyBool)) c in
          let c = Constraints.add (ConstrEqual (d0, d1)) c in
          let c = Constraints.add (ConstrEqual (d1, d2)) c in
          t, a, b, c
    in
    (* logging *)
    (*
    print_endline @@ Printf.sprintf "%s; |- %s: %s; %s" (string_of_type a) (string_of_exp e) (string_of_type t) (string_of_type b);
    print_endline @@ string_of_constraints c;
    *)
    t, a, b, c
  in
  generate_constraints env e

let unify constraints : substitutions =
  let rec unify c =
    match c with
    | [] -> []
    | constr :: c -> begin
      match constr with
      | ConstrConsistent (u1, u2) when u1 = u2 && is_bvp_type u1 ->
          unify c
      | ConstrConsistent (TyDyn, _)
      | ConstrConsistent (_, TyDyn) ->
          unify c
      | ConstrConsistent (TyFun (u11, u12, u13, u14), TyFun (u21, u22, u23, u24)) ->
          unify @@ ConstrConsistent (u11, u21) :: ConstrConsistent (u12, u22) :: ConstrConsistent (u13, u23) :: ConstrConsistent (u14, u24) :: c
      | ConstrConsistent (u, TyVar x) when not @@ is_tyvar u ->
          unify @@ ConstrConsistent (TyVar x, u) :: c
      | ConstrConsistent (TyVar x, u) when is_bvp_type u ->
          unify @@ ConstrEqual (TyVar x, u) :: c
      | ConstrConsistent (TyVar x, TyFun (u1, u2, u3, u4)) when not @@ Int.Set.mem (tyvars (TyFun (u1, u2, u3, u4))) x ->
          let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
          unify @@ ConstrEqual (TyVar x, TyFun (x1, x2, x3, x4)) :: ConstrConsistent (x1, u1) :: ConstrConsistent (x2, u2) :: ConstrConsistent (x3, u3) :: ConstrConsistent (x4, u4) :: c
      | ConstrEqual (t1, t2) when t1 = t2 && is_static_type t1 && is_bvp_type t1 ->
          unify c
      | ConstrEqual (TyFun (t11, t12, t13, t14), TyFun (t21, t22, t23, t24)) when is_static_types [t11; t12; t13; t14; t21; t22; t23; t24] ->
          unify @@ ConstrEqual (t11, t21) :: ConstrEqual (t12, t22) :: ConstrEqual (t13, t23) :: ConstrEqual (t14, t24) :: c
      | ConstrEqual (t, TyVar x) when is_static_type t && not (is_tyvar t) ->
          unify @@ ConstrEqual (TyVar x, t) :: c
      | ConstrEqual (TyVar x, t) when not @@ Int.Set.mem (tyvars t) x ->
          let s = unify @@ subst_type_constraints x t c in
          (x, t) :: s
      | _ ->
          raise @@ Type_error ("cannot unify: " ^ (string_of_constr constr))
    end
  in
  unify @@ map_constraints (fun x -> x) constraints

let type_of_exp env e =
  let u, a, b, c = generate_constraints env e in
  let s = unify c in
  let t = subst_type_substitutions u s in
  subst_tyvars t
