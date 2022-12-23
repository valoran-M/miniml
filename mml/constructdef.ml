open Mml

let rec verif_no_tvar_struct name = function
  | [] -> ()
  | (_, TVar t, _, loc) :: _ -> 
      Error.struct_def_error name loc t
  | _ :: l -> verif_no_tvar_struct name l

let rec verif_only_tvar_type_constr name s l =
  let rec aux = function
  | [] -> ()
  | (TVar ts, l) :: _ when ts <> s -> 
      Error.constr_def_error name l ts
  | _ :: l -> aux l
  in
  match l with 
  | [] -> ()
  | (_, l) :: lt -> aux l; verif_only_tvar_type_constr name s lt

let rec verif_construct = function
  | [] -> ()
  | (name, StrctDef s) :: l -> 
      verif_no_tvar_struct name s;
      verif_construct l
  | (name, ConstrDef (l, None)) :: lt  -> 
      verif_only_tvar_type_constr name "" l;
      verif_construct lt
  | (name, ConstrDef (l, Some s)) :: lt -> 
      verif_only_tvar_type_constr name s l;
      verif_construct lt
