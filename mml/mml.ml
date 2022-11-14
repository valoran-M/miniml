type bop =
    | Add
    | Mul

type expr =
    | Int of int
    | Bop of bop * expr * expr

type prog = { code : expr }
