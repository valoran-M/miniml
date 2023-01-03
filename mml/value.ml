open Mml

type value =
    | VInt    of int
    | VBool   of bool
    | VChar   of char
    | VString of string
    | VPtr    of int
    | VUnit

(* Environnement : associe des valeurs à des noms de variables *)
module Env = Map.Make (String)

(* Élements du tas *)
type heap_value =
    | VClos   of string * expr_loc * value Env.t
    | VStrct  of (string, value) Hashtbl.t
    | VConstr of string * value list
    | VArray  of value array

(* Affiche les valeurs retourner par eval_prog *)
let rec value_to_string ?a:(a = false) ?q:(q = false) mem = function
  | VInt n            -> string_of_int n
  | VBool b           -> string_of_bool b
  | VChar c when q    -> Printf.sprintf "'%c'" c
  | VChar c           -> Printf.sprintf "%c" c
  | VString s when q  -> Printf.sprintf "\"%s\"" s
  | VString s         -> s
  | VUnit             -> Printf.sprintf "()"
  | VPtr p when a     -> 
      Printf.sprintf "@%d -> %s" p (value_in_mem_to_string a p mem)
  | VPtr p            -> Printf.sprintf "%s" (value_in_mem_to_string a p mem)

and value_in_mem_to_string a p mem =
  match Hashtbl.find mem p with
  | VClos (s, _, _)   -> Printf.sprintf "%s" s
  | VStrct s          -> print_struct a mem s
  | VConstr (s, v)    -> 
      Printf.sprintf "%s ( %s" s (list_value_to_string a mem v)
  | VArray t ->
      Printf.sprintf
        "[| %s |]"
        (Array.fold_left
           (fun acc v -> Printf.sprintf "%s; %s" 
            acc (value_to_string ~a:a mem v))
          "" t)

and print_struct a mem s =
  Printf.printf "{ ";
  Hashtbl.fold
    (fun id v s -> 
      Printf.sprintf "%s = %s; %s" id (value_to_string ~a:a mem v) s)
    s ""

and list_value_to_string a mem = function
  | []      -> Printf.sprintf ")"
  | [ v ]   -> Printf.sprintf "%s)" (value_to_string ~a:a mem v)
  | v :: l  ->
      Printf.sprintf
        "%s, %s"
        (value_to_string ~a:a mem v)
        (list_value_to_string a mem l)
