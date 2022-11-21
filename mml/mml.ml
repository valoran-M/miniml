type uop = 
    | Neg 
    | Not

type bop =
  (* opérations arithmétiques *)
    | Add | Sub
    | Mod
    | Mul | Div
  (* opérations booléennes *)
    | Equ | Nequ | Le | Lt
    | Or
    | And

type expr =
    | Int   of int
    | Bool  of bool
    | Unit
    | Uop   of uop * expr 
    | Bop   of bop * expr * expr
    | If    of expr * expr * expr
    | Seq   of expr * expr

type prog = { code : expr }
