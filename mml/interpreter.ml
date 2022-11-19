open Mml

type value =
    | VInt of int
    | VBool of bool

let eval_prog (prog : prog) =
  let rec evali (e : expr) : int =
    match e with
    | Int n -> n
    | _ -> assert false
  in

  let rec eval (e : expr) : value =
    match e with
    | Int n -> VInt n
    | Bool b -> VBool b
    | Uop (Neg, e) -> VInt (-evali e)
    | Bop (Add, e1, e2) -> VInt (evali e1 + evali e2)
    | Bop (Sub, e1, e2) -> VInt (evali e1 - evali e2)
    | Bop (Mul, e1, e2) -> VInt (evali e1 * evali e2)
    | Bop (Div, e1, e2) -> VInt (evali e1 / evali e2)
    | Seq (e1, e2) ->
        let _ = eval e1 in
        eval e2
  in
  eval prog.code
