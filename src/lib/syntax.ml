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
        let s1 = sprintf (match t1 with TyFun _ -> "(%s)" | _ -> "%s") @@ string_of_type t1 in
        let s2 = sprintf (match t2 with TyFun _ -> "(%s)" | _ -> "%s") @@ string_of_type t2 in
        let s3 = sprintf (match t3 with TyFun _ -> "(%s)" | _ -> "%s") @@ string_of_type t3 in
        let s4 = sprintf (match t4 with TyFun _ -> "(%s)" | _ -> "%s") @@ string_of_type t4 in
        sprintf "%s/%s -> %s/%s" s1 s2 s3 s4
    | TyDyn -> "?"
  in
  string_of_type t

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

type binop =
  | Plus
  | Minus

(* binop -> string *)
let string_of_binop = function
  | Plus -> "+"
  | Minus -> "-"

type exp =
  | Var of id
  | Const of const
  | BinOp of binop * exp * exp
  | Fun of ty option * id * ty option * exp (* λ^12:3.4 *)
  | App of exp * exp
  | Shift of id * ty option * exp (* S1:2.3 *)
  | Reset of exp * ty option (* <1>^2 *)
  | If of exp * exp * exp

(* string -> ty option -> string *)
let string_of_type_annot x = function
  | None -> x
  | Some t -> sprintf "(%s: %s)" x @@ string_of_type t

(* ty option -> string *)
let string_of_answer_type_annot = function
  | None -> ""
  | Some t -> sprintf "^%s" @@ string_of_type t

(* exp -> string *)
let rec string_of_exp = function
  | Var id -> id
  | Const c -> string_of_const c
  | BinOp (op, e1, e2) ->
      sprintf "%s %s %s" (string_of_exp e1) (string_of_binop op) (string_of_exp e2)
  | Fun (g, x, x_t, e) ->
      sprintf "fun%s %s -> %s" (string_of_answer_type_annot g) (string_of_type_annot x x_t) (string_of_exp e)
  | App (x, y) -> sprintf "(%s %s)" (string_of_exp x) (string_of_exp y)
  | Shift (k, k_t, e) ->
      sprintf "shift (fun %s -> %s)" (string_of_type_annot k k_t) (string_of_exp e)
  | Reset (e, u) ->
      sprintf "reset (fun () -> %s)%s" (string_of_exp e) (string_of_answer_type_annot u)
  | If _ -> raise @@ Failure "not implemented 1"

(* Type Environment *)

module Environment = Map.Make (
  struct
    type t = id
    let compare (x : id) y = compare x y
  end
)
