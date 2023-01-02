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

let rec construct_find_ret f l ret =
  match l with
  | [] -> raise Not_found
  | (_, StrctDef _) :: l -> construct_find_ret f l ret
  | (name, AlgDef (lc, s)) :: l ->
      let find, acc = (f name lc s ret) in 
      if find then acc 
              else construct_find_ret f l ret

let rec list_find_ret f l ret = 
  match l with
  | [] -> raise Not_found
  | x :: l ->
    let find, acc = (f x ret) in 
    if find then (true, acc) 
            else list_find_ret f l ret
