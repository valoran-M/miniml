open Mml

let rec struct_find f l =
  match l with
  | [] -> raise Not_found
  | (_, AlgDef _) :: l -> struct_find f l
  | (name, StrctDef s) :: l ->
      if f s then
        name
      else
        struct_find f l

let rec construct_find f l =
  match l with
  | [] -> raise Not_found
  | (_, StrctDef _) :: l -> construct_find f l
  | (name, AlgDef (lc, s)) :: l ->
      if f lc then
        (name, s)
      else
        construct_find f l
