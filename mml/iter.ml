open Mml

let rec struct_find f l =
  match l with
  | [] -> raise Not_found
  | (_, ConstrDef _) :: l -> struct_find f l
  | (name, StrctDef s) :: l ->
      if f s then
        name
      else
        struct_find f l

let rec construct_find f l =
  match l with
  | [] -> raise Not_found
  | (_, StrctDef _) :: l -> construct_find f l
  | (name, ConstrDef (uident, c)) :: l ->
      if f uident c then
        name
      else
        construct_find f l
