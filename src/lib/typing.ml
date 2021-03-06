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
let is_static_types types = List.fold_left (&&) true @@ List.map is_static_type types

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

module Variables = Set.Make(
  struct
    type t = tyvar
    let compare (x : tyvar) y = compare x y
  end
)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    TyVar (v + 1)
  in body

let rec tyvars = function
  | TyVar x -> Variables.singleton x
  | TyFun (t1, t2, t3, t4) ->
      let vars = Variables.union (tyvars t1) (tyvars t2) in
      let vars = Variables.union vars (tyvars t3) in
      let vars = Variables.union vars (tyvars t4) in
      vars
  | _ -> Variables.empty

(* Substitutions *)

type substitution = tyvar * ty
type substitutions = substitution list

let string_of_substitution (x, t) =
  Printf.sprintf "x%d=%s" x @@ string_of_type t

let string_of_substitutions s =
  String.concat ", " @@ List.map string_of_substitution s

let subst_type_substitutions (t : ty) (s : substitutions) =
  List.fold_left (fun u -> fun (x, t) -> subst_type x t u) t s

let fresh_typaram =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    TyParam (v + 1)
  in body

module TyVarMap = Map.Make(
  struct
    type t = tyvar
    let compare (x : tyvar) y = compare x y
  end
)

let rec subst_tyvar t m = match t with
  | TyVar x -> TyVarMap.find x m
  | TyFun (t1, t2, t3, t4) -> TyFun (subst_tyvar t1 m, subst_tyvar t2 m, subst_tyvar t3 m, subst_tyvar t4 m)
  | _ -> t

(* Create map from type parameters to type variables *)
let create_tyvar_typaram_map t =
  let f x m = if TyVarMap.mem x m then m else TyVarMap.add x (fresh_typaram ()) m in
  Variables.fold f (tyvars t) TyVarMap.empty

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
  let rec generate_constraints env e b =
    let t, a, c = match e with
      | Var x ->
          let u_a = b in (
          try
            let u = Environment.find x env in
            u, u_a, Constraints.empty
          with Not_found ->
            raise @@ Type_error (Printf.sprintf "variable '%s' not found in the environment" x)
          )
      | Const c ->
          let u_a = b in
          let u = begin
            match c with
            | ConstBool b -> TyBool
            | ConstInt i -> TyInt
            | ConstUnit -> TyUnit
            end
          in
          u, u_a, Constraints.empty
      | BinOp (op, e1, e2) ->
          let u_a0 = b in
          let u1, u_a1, c1 = generate_constraints env e1 u_a0 in
          let u2, u_a2, c2 = generate_constraints env e2 u_a1 in
          let c = Constraints.union c1
                  @@ Constraints.union c2
                  @@ Constraints.add (ConstrConsistent (u1, TyInt))
                  @@ Constraints.singleton
                  @@ ConstrConsistent (u2, TyInt) in
          TyInt, u_a2, c
      | Fun (None, x, None, e) ->
          let u_a = b in
          let x_x, x_g = fresh_tyvar (), fresh_tyvar () in
          let u, u_b, c = generate_constraints (Environment.add x x_x env) e x_g in
          TyFun (x_x, u_b, u, x_g), u_a, c
      | Fun (None, x, Some u_1, e) ->
          let u_a = b in
          let x_g = fresh_tyvar () in
          let u_2, u_b, c = generate_constraints (Environment.add x u_1 env) e x_g in
          TyFun (u_1, u_b, u_2, x_g), u_a, c
      | Fun (Some u_g, x, None, e) ->
          let u_a = b in
          let x_x = fresh_tyvar () in
          let u_2, u_b, c = generate_constraints (Environment.add x x_x env) e u_g in
          TyFun (x_x, u_b, u_2, u_g), u_a, c
      | Fun (Some u_g, x, Some u_1, e) ->
          let u_a = b in
          let u_2, u_b, c = generate_constraints (Environment.add x u_1 env) e u_g in
          TyFun (u_1, u_b, u_2, u_g), u_a, c
      | App (e1, e2) ->
          let u_d = b in
          let u_1, u_g, c1 = generate_constraints env e1 u_d in
          let u_2, u_b, c2 = generate_constraints env e2 u_g in
          let u, c3 = generate_constraints_domc_eq u_1 in
          let u_a, c4 = generate_constraints_codc_eq u_1 in
          let c5 = generate_constraints_codf_con u_1 u_b in
          let c6 = generate_constraints_domf_con u_1 u_2 in
          let c = Constraints.union c1
                  @@ Constraints.union c2
                  @@ Constraints.union c3
                  @@ Constraints.union c4
                  @@ Constraints.union c5 c6 in
          u, u_a, c
      | Shift (k, None, e) ->
          let u_b = b in
          let x_x, x_a, x_g = fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
          let env' = Environment.add k (TyFun (x_x, x_g, x_a, x_g)) env in
          let u_d, u_d', c = generate_constraints env' e u_b in
          let c = Constraints.add (ConstrConsistent (u_d, u_d')) c in
          x_x, x_a, c
      | Shift (k, Some u_s, e) ->
          let u_b = b in
          let u_d, u_d', c1 = generate_constraints (Environment.add k u_s env) e u_b in
          let u_a, c2 = generate_constraints_domc_eq u_s in
          let u, c3 = generate_constraints_domf_eq u_s in
          let u_g1, c4 = generate_constraints_codc_eq u_s in
          let u_g2, c5 = generate_constraints_codf_eq u_s in
          let _, c6 = generate_constraints_join u_g1 u_g2 in
          let c = Constraints.union c1
                  @@ Constraints.union c2
                  @@ Constraints.union c3
                  @@ Constraints.union c4
                  @@ Constraints.union c5
                  @@ Constraints.union c6
                  @@ Constraints.singleton
                  @@ ConstrConsistent (u_d, u_d') in
          u, u_a, c
      | Reset (e, None) ->
          let u_a = b in
          let x_x = fresh_tyvar () in
          let u_b, u_b', c = generate_constraints env e x_x in
          let c = Constraints.add (ConstrConsistent (u_b, u_b')) c in
          x_x, u_a, c
      | Reset (e, Some u) ->
          let u_a = b in
          let u_b, u_b', c = generate_constraints env e u in
          let c = Constraints.add (ConstrConsistent (u_b, u_b')) c in
          u, u_a, c
      | If (e1, e2, e3) ->
          let u_b = b in
          let u_1, u_d, c1 = generate_constraints env e1 u_b in
          let u_2, u_a2, c2 = generate_constraints env e2 u_d in
          let u_3, u_a3, c3 = generate_constraints env e3 u_d in
          let u_a, c4 = generate_constraints_join u_a2 u_a3 in
          let u, c5 = generate_constraints_join u_2 u_3 in
          let c = Constraints.union c1
                  @@ Constraints.union c2
                  @@ Constraints.union c3
                  @@ Constraints.union c4
                  @@ Constraints.union c5
                  @@ Constraints.singleton
                  @@ ConstrConsistent (u_1, TyBool) in
          u, u_a, c
      | Consq (e1, e2) ->
          let u_g = b in
          let u_1, u_b, c1 = generate_constraints env e1 u_g in
          let u_2, u_a, c2 = generate_constraints env e2 u_b in
          let c = Constraints.union c1
                  @@ Constraints.union c2
                  @@ Constraints.singleton
                  @@ ConstrConsistent (u_1, TyUnit) in
          u_2, u_a, c
    in
    (* logging *)
    (*
    print_endline @@ Printf.sprintf "%s; |- %s: %s; %s" (string_of_type a) (string_of_exp e) (string_of_type t) (string_of_type b);
    print_endline @@ string_of_constraints c;
    *)
    t, a, c
  in
  generate_constraints env e @@ fresh_tyvar ()

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
      | ConstrConsistent (TyVar x, TyFun (u1, u2, u3, u4)) when not @@ Variables.mem x (tyvars (TyFun (u1, u2, u3, u4))) ->
          let x1, x2, x3, x4 = fresh_tyvar (), fresh_tyvar (), fresh_tyvar (), fresh_tyvar () in
          unify @@ ConstrEqual (TyVar x, TyFun (x1, x2, x3, x4)) :: ConstrConsistent (x1, u1) :: ConstrConsistent (x2, u2) :: ConstrConsistent (x3, u3) :: ConstrConsistent (x4, u4) :: c
      | ConstrEqual (t1, t2) when t1 = t2 && is_static_type t1 && is_bvp_type t1 ->
          unify c
      | ConstrEqual (TyFun (t11, t12, t13, t14), TyFun (t21, t22, t23, t24)) when is_static_types [t11; t12; t13; t14; t21; t22; t23; t24] ->
          unify @@ ConstrEqual (t11, t21) :: ConstrEqual (t12, t22) :: ConstrEqual (t13, t23) :: ConstrEqual (t14, t24) :: c
      | ConstrEqual (t, TyVar x) when is_static_type t && not (is_tyvar t) ->
          unify @@ ConstrEqual (TyVar x, t) :: c
      | ConstrEqual (TyVar x, t) when not @@ Variables.mem x (tyvars t) ->
          let s = unify @@ subst_type_constraints x t c in
          (x, t) :: s
      | _ ->
          raise @@ Type_error ("cannot unify: " ^ (string_of_constr constr))
    end
  in
  unify @@ map_constraints (fun x -> x) constraints

let type_of_exp env e =
  let u, a, c = generate_constraints env e in
  let s = unify c in
  let t = subst_type_substitutions u s in
  subst_tyvars t
