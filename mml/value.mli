open Mml

type value =
    | VInt    of int
    | VBool   of bool
    | VChar   of char
    | VString of string
    | VPtr    of int
    | VUnit

module Env : Map.S with type key = string

type heap_value =
    | VClos   of string * expr_loc * value Env.t
    | VStrct  of (string, value) Hashtbl.t
    | VConstr of string * value list
    | VArray  of value array

(** affiche une valeur si c'est une adresse le contenu est aussi affiché 
    ?a = affiche les adresse si true
    ?q = affiche les guillement si true
 *)
val value_to_string : ?a:bool -> ?q:bool -> (int, heap_value) Hashtbl.t -> value -> string
