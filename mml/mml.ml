type typ =
    | TInt
    | TBool
    | TUnit
    | TVar    of string
    | TFun    of typ * typ
    | TDef    of string


type enum = string list

type tDef = 
    | StrctDef      of (string * typ * bool)  list
    | ConstrDef     of (string * typ list)    list

type uop = Neg | Not

type bop =
    (* opérations arithmétiques *)
    | Add   | Sub
    | Mod
    | Mul   | Div
    (* opérations booléennes *)
    | Equ   | Nequ
    | Sequ  | Snequ
    | Le    | Lt
    | Or    | And

type location = 
  { fc  : Lexing.position
  ; lc  : Lexing.position}

type expr =
    | Int     of int
    | Bool    of bool
    | Unit
    | Uop     of uop * expr_loc
    | Bop     of bop * expr_loc * expr_loc
    | Var     of string
    | If      of expr_loc * expr_loc * expr_loc option
    | Let     of string * expr_loc * typ option * expr_loc
    | Fun     of string * typ option * expr_loc
    | App     of expr_loc * expr_loc
    | Fix     of string * typ option * expr_loc
    | Strct   of (string * expr_loc) list
    | GetF    of expr_loc * string
    | SetF    of expr_loc * string * expr_loc
    | Seq     of expr_loc * expr_loc
    | Constr  of string * expr_loc list

and expr_loc =
  { loc : location
  ; expr: expr}

type prog = 
  {types : (string * tDef) list
  ; code : expr_loc}
