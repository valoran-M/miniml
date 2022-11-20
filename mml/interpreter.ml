open Mml

type value =
    | VInt of int
    | VBool of bool

let eval_prog (prog : prog) =
  let rec evali (e : expr) : int =
    match e with
    | Int n -> n
    | Uop (Neg, e) -> -evali e
    | Bop (Add, e1, e2) -> evali e1 + evali e2
    | Bop (Sub, e1, e2) -> evali e1 - evali e2
    | Bop (Mod, e1, e2) -> evali e1 mod evali e2
    | Bop (Mul, e1, e2) -> evali e1 * evali e2
    | Bop (Div, e1, e2) -> evali e1 / evali e2
    | _ -> assert false
  in
  let rec evalb (e : expr) : bool =
    match e with
    | Bool b -> b
    | Uop (Not, e) -> not (evalb e)
    | Bop (Equ, e1, e2) -> evali e1 = evali e2
    | Bop (Nequ, e1, e2) -> evali e1 != evali e2
    | Bop (Le, e1, e2) -> evali e1 <= evali e2
    | Bop (Lt, e1, e2) -> evali e1 < evali e2
    | Bop (Or, e1, e2) -> evalb e1 || evalb e2
    | Bop (And, e1, e2) -> evalb e1 && evalb e2
    | _ -> assert false
  in

  let rec eval (e : expr) : value =
    match e with
    | Bool b -> VBool b
    | ( Uop (Not, _)
      | Bop (Equ, _, _)
      | Bop (Nequ, _, _)
      | Bop (Le, _, _)
      | Bop (Lt, _, _)
      | Bop (Or, _, _)
      | Bop (And, _, _) ) as op -> VBool (evalb op)
    | Int n -> VInt n
    | ( Uop (Neg, _)
      | Bop (Add, _, _)
      | Bop (Sub, _, _)
      | Bop (Mod, _, _)
      | Bop (Mul, _, _)
      | Bop (Div, _, _) ) as op -> VInt (evali op)
    | Seq (e1, e2) ->
        let _ = eval e1 in
        eval e2
  in
  eval prog.code
