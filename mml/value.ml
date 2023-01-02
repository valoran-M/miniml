open Mml

type value =
    | VInt of int
    | VBool of bool
    | VUnit
    | VPtr of int

(* Environnement : associe des valeurs à des noms de variables *)
module Env = Map.Make (String)

(* Élements du tas *)
type heap_value =
    | VClos of string * expr_loc * value Env.t
    | VStrct of (string, value) Hashtbl.t
    | VConstr of string * value list
    | VArray of value array

(* Affiche les valeurs retourner par eval_prog *)
let rec value_to_string mem = function
  | VInt n -> Printf.sprintf "%d" n
  | VBool b -> Printf.sprintf "%b" b
  | VUnit -> Printf.sprintf "()"
  | VPtr p -> Printf.sprintf "@%d -> %s" p (value_in_mem_to_string p mem)

and value_in_mem_to_string p mem =
  match Hashtbl.find mem p with
  | VClos (s, _, _) -> Printf.sprintf "%s" s
  | VStrct s -> print_struct mem s
  | VConstr (s, v) -> Printf.sprintf "%s ( %s" s (list_value_to_string mem v)
  | VArray a ->
      Printf.sprintf
        "[| %s |]"
        (Array.fold_left
           (fun acc v -> Printf.sprintf "%s; %s" acc (value_to_string mem v))
           "" a)

and print_struct mem s =
  Printf.printf "{ ";
  Hashtbl.fold
    (fun id v s -> Printf.sprintf "%s = %s; %s" id (value_to_string mem v) s)
    s
    ""

and list_value_to_string mem = function
  | [] -> Printf.sprintf ")"
  | [ v ] -> Printf.sprintf "%s)" (value_to_string mem v)
  | v :: l ->
      Printf.sprintf
        "%s, %s"
        (value_to_string mem v)
        (list_value_to_string mem l)
