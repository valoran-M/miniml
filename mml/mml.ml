type typ =
    | TInt
    | TBool
    | TUnit
    | TFun of typ * typ

type uop =
    | Neg   | Not

type bop =
    (* opérations arithmétiques *)
    | Add   | Sub
    | Mod
    | Mul   | Div
    (* opérations booléennes *)
    | Equ   | Nequ
    | Le    | Lt
    | Or    | And

type expr =
    | Int of int
    | Bool of bool
    | Unit
    | Uop of uop * expr
    | Bop of bop * expr * expr
    | If of expr * expr * expr
    | Seq of expr * expr
    | Var of string
    | Fun of string * typ * expr
    | Let of string * expr * expr
    | App of expr * expr
    | Fix   of string * typ * expr

type prog = { code : expr }
