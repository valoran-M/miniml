type uop = Neg

type bop =
    | Add | Sub
    | Mul | Div

type expr =
    | Int   of int
    | Bool  of bool
    | Uop   of uop * expr 
    | Bop   of bop * expr * expr
    | Seq   of expr * expr

type prog = { code : expr }
