type value =
    | VInt of int
    | VBool of bool
    | VUnit
    | VPtr of int

(* Environnement : associe des valeurs à des noms de variables *)
module Env : Map.S with type key = string

(* Élements du tas *)
type heap_value =
    | VClos of string * Mml.expr_loc * value Env.t
    | VStrct of (string, value) Hashtbl.t
    | VConstr of string * value list

val print_value : (int, heap_value) Hashtbl.t -> value -> unit

val eval_prog : Mml.prog -> value * (int, heap_value) Hashtbl.t
