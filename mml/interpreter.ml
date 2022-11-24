open Mml

(* Environnement : associe des valeurs à des noms de variables *)
module Env = Map.Make (String)

(* Valeurs *)
type value =
    | VInt  of int
    | VBool of bool
    | VUnit
    | VPtr  of int

let print_value = function
  | VInt n  -> Printf.printf "%d\n" n
  | VBool b -> Printf.printf "%b\n" b
  | VUnit   -> Printf.printf "()\n"
  | VPtr p  -> Printf.printf "@%d\n" p

(* Élements du tas *)
type heap_value =
    | VClos   of string * expr * value Env.t
    | VStrct  of (string, value) Hashtbl.t

(* Interprétation d'un programme complet *)
let eval_prog (prog : prog) : value =
  (* Initialisation de la mémoire globale *)
  let (mem : (int, heap_value) Hashtbl.t) = Hashtbl.create 16 in

  (* Création de nouvelles adresses *)
  let new_ptr =
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      !cpt
  in

  (* Interprétation d'une expression, en fonction d'un environnement
     et de la mémoire globale *)
  let rec eval (e : expr) (env : value Env.t) : value =
    match e with
    | Unit -> VUnit
    | Var id -> Env.find id env
    | Fun (id, _, e) ->
        let ptr = new_ptr () in
        Hashtbl.add mem ptr (VClos (id, e, env));
        VPtr ptr
    | Let (id, e1, e2) -> eval e2 (Env.add id (eval e1 env) env)
    | App (e1, e2) -> (
        let val_e2 = eval e2 env in
        match eval e1 env with
        | VPtr p -> (
            match Hashtbl.find mem p with
            | VClos (id, e, env) -> eval e (Env.add id val_e2 env)
            | _ -> assert false)
        | _ -> assert false)
    | Fix (_, _, _) -> assert false (*à compléter *)
    | Bool b -> VBool b
    | ( Uop (Not, _)
      | Bop (Equ, _, _)
      | Bop (Nequ, _, _)
      | Bop (Le, _, _)
      | Bop (Lt, _, _)
      | Bop (Or, _, _)
      | Bop (And, _, _) ) as op -> VBool (evalb op env)
    | Int n -> VInt n
    | ( Uop (Neg, _)
      | Bop (Add, _, _)
      | Bop (Sub, _, _)
      | Bop (Mod, _, _)
      | Bop (Mul, _, _)
      | Bop (Div, _, _) ) as op -> VInt (evali op env)
    | If (c, e1, e2) ->
        if evalb c env then
          eval e1 env
        else
          eval e2 env
    | Strct _ -> assert false (* à compléter *)
    | GetF _ -> assert false (* à compléter *)
    | Seq (e1, e2) ->
        let _ = eval e1 env in
        eval e2 env
  (* Évaluation d'une expression dont la valeur est supposée entière *)
  and evali (e : expr) (env : value Env.t) : int =
    match e with
    | Int n -> n
    | Uop (Neg, e) -> -evali e env
    | Bop (Add, e1, e2) -> evali e1 env + evali e2 env
    | Bop (Sub, e1, e2) -> evali e1 env - evali e2 env
    | Bop (Mod, e1, e2) -> evali e1 env mod evali e2 env
    | Bop (Mul, e1, e2) -> evali e1 env * evali e2 env
    | Bop (Div, e1, e2) -> evali e1 env / evali e2 env
    | e -> (
        match eval e env with
        | VInt n -> n
        | _ -> assert false)
  (* Évaluation d'une expression dont la valeur est supposée booléenne *)
  and evalb (e : expr) (env : value Env.t) : bool =
    match e with
    | Bool b -> b
    | Uop (Not, e) -> not (evalb e env)
    | Bop (Equ, e1, e2) -> evali e1 env == evali e2 env
    | Bop (Nequ, e1, e2) -> evali e1 env != evali e2 env
    | Bop (Le, e1, e2) -> evali e1 env <= evali e2 env
    | Bop (Lt, e1, e2) -> evali e1 env < evali e2 env
    | Bop (Or, e1, e2) -> evalb e1 env || evalb e2 env
    | Bop (And, e1, e2) -> evalb e1 env && evalb e2 env
    | e -> (
        match eval e env with
        | VBool b -> b
        | _ -> assert false)
  in
  eval prog.code Env.empty
