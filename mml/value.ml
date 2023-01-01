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
let rec print_value mem = function
  | VInt n -> Printf.printf "%d" n
  | VBool b -> Printf.printf "%b" b
  | VUnit -> Printf.printf "()"
  | VPtr p ->
      Printf.printf "@%d ->" p;
      print_value_in_mem p mem

and print_value_in_mem p mem =
  match Hashtbl.find mem p with
  | VClos (s, _, _) -> Printf.printf "%s" s
  | VStrct s -> print_struct mem s
  | VConstr (s, v) ->
      Printf.printf "%s (" s;
      print_list_value mem v
  | VArray a ->
      print_string "[|";
      for i = 0 to Array.length a - 2 do
        print_value mem a.(i);
        print_string "; "
      done;
      print_value mem a.(Array.length a - 1);
      print_string "|]\n"

and print_struct mem s =
  Printf.printf "{ ";
  Hashtbl.iter
    (fun id v ->
      Printf.printf "%s = " id;
      print_value mem v;
      Printf.printf "; ")
    s;
  print_char '}'

and print_list_value mem = function
  | [] -> print_char ')'
  | [ v ] ->
      print_value mem v;
      print_char ')'
  | v :: l ->
      print_value mem v;
      print_string ", ";
      print_list_value mem l
