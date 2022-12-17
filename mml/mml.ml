type typ =
    | TInt
    | TBool
    | TUnit
    | TVar    of string
    | TFun    of typ * typ
    | TStrct  of string


type enum = string list

type tDef = 
    | StrctDef of (string * typ * bool) list
    | EnumDef  of string list

type uop = Neg | Not

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
    | Int   of int
    | Bool  of bool
    | Unit
    | Uop   of uop * expr
    | Bop   of bop * expr * expr
    | Var   of string
    | If    of expr * expr * expr
    | Let   of string * expr * expr
    | Fun   of string * typ * expr
    | App   of expr * expr
    | Fix   of string * typ * expr
    | Strct of (string * expr) list
    | GetF  of expr * string
    | SetF  of expr * string * expr
    | Seq   of expr * expr

type prog = {
    types : (string * tDef) list;
    code : expr;
  }
