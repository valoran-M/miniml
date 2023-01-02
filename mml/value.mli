open Mml

type value =
    | VInt of int
    | VBool of bool
    | VUnit
    | VPtr of int

module Env : Map.S with type key = string

type heap_value =
    | VClos of string * expr_loc * value Env.t
    | VStrct of (string, value) Hashtbl.t
    | VConstr of string * value list
    | VArray of value array

(** affiche une valeur si c'est une adresse le contenu est aussi affiché *)
val value_to_string : (int, heap_value) Hashtbl.t -> value -> string
