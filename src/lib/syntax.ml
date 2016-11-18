open Core.Std
open Printf

(* Types *)

type typaram = int
type tyvar = int

type ty =
  | TyParam of typaram
  | TyVar of tyvar
  | TyBool
  | TyInt
  | TyUnit
  | TyFun of ty * ty * ty * ty
  | TyDyn

let string_of_type t =
  let params = ref [] in
  let string_of_typaram tp =
    let rec string_of_typaram i = function
      | [] -> params := !params @ [tp]; i
      | x :: _ when x = tp -> i
      | _ :: params -> string_of_typaram (i + 1) params
    in
    let i = string_of_typaram 0 !params in
    "'" ^ String.make 1 @@ char_of_int @@ (int_of_char 'a') + i
  in
  let rec string_of_type = function
    | TyParam tp -> string_of_typaram tp
    | TyVar x -> "'x" ^ string_of_int x
    | TyBool -> "bool"
    | TyInt -> "int"
    | TyUnit -> "unit"
    | TyFun (t1, t2, t3, t4) ->
        let s1 = string_of_type t1 in
        let s2 = string_of_type t2 in
        let s3 = string_of_type t3 in
        let s4 = string_of_type t4 in
(*         let s1 = (match t1 with TyFun _ -> sprintf "(%s)" s1 | _ -> s1) in *)
        sprintf "(%s/%s -> %s/%s)" s1 s2 s3 s4
    | TyDyn -> "?"
  in
  string_of_type t

let string_of_type2 t =
  let params = ref [] in
  let string_of_typaram tp =
    let rec string_of_typaram i = function
      | [] -> params := !params @ [tp]; i
      | x :: _ when x = tp -> i
      | _ :: params -> string_of_typaram (i + 1) params
    in
    let i = string_of_typaram 0 !params in
    "'" ^ String.make 1 @@ char_of_int @@ (int_of_char 'a') + i
  in
  let rec string_of_type2 = function
    | TyParam tp -> string_of_typaram tp
    | TyVar x -> "'x" ^ string_of_int x
    | TyBool -> "bool"
    | TyInt -> "int"
    | TyUnit -> "unit"
    | TyFun (t1, t2, t3, t4) ->
        let s1 = string_of_type2 t1 in
        let s3 = string_of_type2 t3 in
        let s1 = (match t1 with TyFun _ -> sprintf "(%s)" s1 | _ -> s1) in
        sprintf "%s -> %s" s1 s3
    | TyDyn -> "?"
  in
  string_of_type2 t

(* Syntax *)

type id = string

type const =
  | ConstBool of bool
  | ConstInt of int
  | ConstUnit

let string_of_const = function
  | ConstBool b -> string_of_bool b
  | ConstInt i -> string_of_int i
  | ConstUnit -> "()"

type binop = Plus

type exp =
  | Var of id
  | Const of const
  | BinOp of binop * exp * exp
  | FunI of id * exp
  | FunE of id * ty * exp
  | App of exp * exp
  | ShiftI of id * exp
  | ShiftE of id * ty * exp
  | ResetI of exp
  | If of exp * exp * exp

let rec string_of_exp = function
  | Var id -> id
  | Const c -> string_of_const c
  | BinOp (op, e1, e2) ->
      sprintf "%s + %s" (string_of_exp e1) (string_of_exp e2)
  | FunI (x, e) -> sprintf "fun %s -> %s" x (string_of_exp e)
  | FunE (x, x_t, e) -> sprintf "fun (%s: %s) -> %s" x (string_of_type x_t) (string_of_exp e)
  | App (x, y) -> sprintf "(%s %s)" (string_of_exp x) (string_of_exp y)
  | ShiftI (k, e) -> sprintf "shift (fun %s -> %s)" k (string_of_exp e)
  | ShiftE (k, k_t, e) -> sprintf "shift (fun (%s: %s) -> %s)" k (string_of_type k_t) (string_of_exp e)
  | ResetI e -> sprintf "reset (fun () -> %s)" @@ string_of_exp e
  | If _ -> raise @@ Failure "not implemented 1"
