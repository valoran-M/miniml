open Mml

let verif_struct s name =
  let f acc s =
    match s with
    | _, TVar t, _, loc ->
        Error.struct_def_error loc (Error.struct_undefined_type name t)
    | s, _, _, loc ->
        if List.mem s acc then
          Error.struct_def_error loc (Error.struct_same_label_name s)
        else
          s :: acc
  in
  ignore (List.fold_left f [] s);
  ()

let verif_only_tvar_type_constr name s l =
  let f = function
    | TVar ts, l when ts <> s -> Error.constr_def_error name l ts
    | _ -> ()
  in
  let alg_verif a = List.iter f (snd a) in
  List.iter alg_verif l

let verif_construct types =
  let f t =
    match t with
    | name, StrctDef s -> verif_struct s name
    | name, ConstrDef (l, None) -> verif_only_tvar_type_constr name "" l
    | name, ConstrDef (l, Some s) -> verif_only_tvar_type_constr name s l
  in
  List.iter f types
