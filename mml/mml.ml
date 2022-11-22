type typ =
    | TInt
    | TBool
    | TUnit
    | TFun of typ * typ

let rec typ_to_string = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TFun (typ1, typ2) ->
      Printf.sprintf "(%s) -> %s" (typ_to_string typ1) (typ_to_string typ2)

type uop =
    | Neg
    | Not

type bop =
    (* opérations arithmétiques *)
    | Add
    | Sub
    | Mod
    | Mul
    | Div
    (* opérations booléennes *)
    | Equ
    | Nequ
    | Le
    | Lt
    | Or
    | And

type expr =
    | Int of int
    | Bool of bool
    | Unit
    | Var of string
    | Fun of string * typ * expr
    | Let of string * expr * expr
    | App of expr * expr
    | Uop of uop * expr
    | Bop of bop * expr * expr
    | If of expr * expr * expr
    | Seq of expr * expr

type prog = { code : expr }
