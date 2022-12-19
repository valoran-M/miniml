type typ =
    | TInt
    | TBool
    | TUnit
    | TVar    of string
    | TFun    of typ * typ
    | TStrct  of string
    | TConstr of string


type enum = string list

type tDef = 
    | StrctDef      of (string * typ * bool) list
    | ConstrDef     of (string * typ list) list

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
    | Int     of int
    | Bool    of bool
    | Unit
    | Uop     of uop * expr
    | Bop     of bop * expr * expr
    | Var     of string
    | If      of expr * expr * expr
    | Let     of string * expr * expr
    | Fun     of string * typ option * expr
    | App     of expr * expr
    | Fix     of string * typ option * expr
    | Strct   of (string * expr) list
    | GetF    of expr * string
    | SetF    of expr * string * expr
    | Seq     of expr * expr
    | Constr  of string * expr list

type prog = {
    types : (string * tDef) list;
    code : expr;
  }
