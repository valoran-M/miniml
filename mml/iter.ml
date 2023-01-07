open Mml

(**
    trouve une structure qui rend vrai f

    @param f (string * typ * bool * location) list -> bool
    @param l ('a * tDef) list

    @return le nom de la structure trouvé

    @raise Not_found si aucune structure n'est trouvé 
  *)
let rec struct_find f l =
  match l with
  | [] -> raise Not_found
  | (_, AlgDef _) :: l -> struct_find f l
  | (name, StrctDef s) :: l ->
      if f s then
        name
      else
        struct_find f l

(**
    trouve un type algebric qui rend vrai f

    @param f fonction qui prend en paramètre touts les éléments 
              d'un type algébrique avec un accumulateur 
              et qui retourne un bool (si trouvé) et l'accumulateur
    @param l list des types
    @param acc

    @return le nom du constructeur et son type paramétrique trouvé

    @raise Not_found si aucun type n'est trouvé 
  *)
let rec construct_find f l =
  match l with
  | [] -> raise Not_found
  | (_, StrctDef _) :: l -> construct_find f l
  | (name, AlgDef (lc, s)) :: l ->
      if f lc then
        (name, s)
      else
        construct_find f l

(**
    trouve un type algebric qui rend vrai f

    @param f fonction qui prend en paramètre touts les éléments 
              d'un type algébrique avec un accumulateur 
              et qui retourne un bool (si trouvé) et l'accumulateur
    @param l list des types
    @param ret argument par défaut de f

    @return le second éléments de f si le premier est vrai

    @raise Not_found si aucun type n'est trouvé 
  *)
let rec construct_find_ret f l ret =
  match l with
  | [] -> raise Not_found
  | (_, StrctDef _) :: l -> construct_find_ret f l ret
  | (name, AlgDef (lc, s)) :: l ->
      let find, acc = (f name lc s ret) in 
      if find then acc 
              else construct_find_ret f l ret

(**
    trouve un élément d'une liste qui rend vrai f 
    et retroune le second retour de f

    @param f fonction qui prend en pramètre le type de la liste 
              et renvoi un bool (si trouvé) et le retour
    @param l la liste
    @param ret le retour par defaut de f

    @return le deuxième élément de f si le premier est vrai

    @raise Not_found si aucun élément n'a été trouvé 
  *)
let rec list_find_ret f l ret = 
  match l with
  | [] -> raise Not_found
  | x :: l ->
    let find, acc = (f x ret) in 
    if find then (true, acc) 
            else list_find_ret f l ret
