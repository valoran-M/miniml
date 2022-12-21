open Mml

(* Environnement de typage : associe des types aux noms de variables *)
module SymTbl = Map.Make (String)

type tenv = typ SymTbl.t

(* Vérification des types d'un programme *)
let type_prog prog =
  let types = List.rev prog.types in

  let get_type_def name =
    snd (List.find (fun (id, _) -> name = id) prog.types)
  in

  let get_struct name =
    match get_type_def name with
    | StrctDef s -> s
    | _ -> assert false
  in

  (* Vérifie que l'expression [e] a le type [type] *)
  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then Error.type_error e typ_e typ
  (* Calcule le type de l'expression [e] *)
  and type_expr e tenv =
    match e.expr with
    | Int _ -> TInt
    | Bool _ -> TBool
    | Unit -> TUnit
    | Uop (Neg, e) ->
        check e TInt tenv;
        TInt
    | Uop (Not, e) ->
        check e TBool tenv;
        TBool
    | Bop ((Add | Sub 
          | Mod 
          | Mul | Div), e1, e2) ->
        check e1 TInt tenv;
        check e2 TInt tenv;
        TInt
    | Bop ((Or | And), e1, e2) ->
        check e1 TBool tenv;
        check e2 TBool tenv;
        TBool
    | Bop ((Equ   | Nequ
          | Sequ  | Snequ), e1, e2) ->
        check e2 (type_expr e1 tenv) tenv;
        TBool
    | Bop ((Lt | Le), e1, e2) -> 
        check e1 TInt tenv;
        check e2 TInt tenv;
        TBool
    | Var s -> SymTbl.find s tenv
    | Let (s, e1, e2) ->
        let t1 = type_expr e1 tenv in
        type_expr e2 (SymTbl.add s t1 tenv)
    | If (c, e1, Some e2) ->
        check c TBool tenv;
        let t2 = type_expr e2 tenv in
        check e1 t2 tenv;
        t2
    | If (c, e, None) ->
        check c TBool tenv;
        check e TUnit tenv;
        TUnit
    | Fun (id, Some t, e) -> 
        TFun (t, type_expr e (SymTbl.add id t tenv))
    | Fun (_, None, _) -> assert false
    | App (e1, e2) -> (
        match type_expr e1 tenv with
        | TFun (t1, t2) ->
            check e2 t1 tenv;
            t2
        | t -> Error.not_a_function e1 t)
    | Fix (s, Some t, e) -> type_expr e (SymTbl.add s t tenv)
    | Fix (_, None, _) -> assert false
    | Seq (e1, e2) ->
        let _ = type_expr e1 tenv in
        type_expr e2 tenv
    | Strct l ->
        let rec stuct_construct = function
          | [] ->
              Error.struct_construct_error e
                (List.map (fun (n, e) -> (n, type_expr e tenv)) l)
          | (_, ConstrDef _) :: ld -> stuct_construct ld
          | (name, StrctDef s) :: ld -> (
              let rec iter_args = function
                | (id1, e) :: l1, (id2, t, _) :: l2 ->
                    if id1 = id2 then
                      if t <> type_expr e tenv then
                        None
                      else
                        iter_args (l1, l2)
                    else
                      None
                | [], [] -> Some name
                | _, _ -> None
              in
              match iter_args (l, s) with
              | Some name -> TDef name
              | None -> stuct_construct ld)
        in
        stuct_construct prog.types
    | GetF (e1, x) -> (
        match type_expr e1 tenv with
        | TDef s -> (
            let st = get_struct s in
            try
              let _, t, _ = (List.find (fun (id, _, _) -> id = x)) st in
              t
            with Not_found -> Error.struct_no_field e s x)
        | t -> Error.not_a_struct e t)
    | SetF (e1, x, e2) -> (
        match type_expr e1 tenv with
        | TDef s -> (
            let st = get_struct s in
            try
              let _, t, m = (List.find (fun (id, _, _) -> id = x)) st in
              check e2 t tenv;
              if m then
                TUnit
              else
                Error.is_not_mutable e s x
            with Not_found -> Error.struct_no_field e s x)
        | t -> Error.not_a_struct e1 t)
    | Constr (name, ex) ->
        let lt2 = List.map (fun e -> (type_expr e tenv, e)) ex in
        let rec iter_args cname = function
          | [] -> None
          | (id, lt1) :: l ->
            if name = id then
              try
                List.iter2
                  (fun t1 (t2, e) ->
                    if t1 <> t2 then Error.type_error e t1 t2)
                  lt1 lt2;
                Some cname
              with Invalid_argument _ ->
                Error.nb_arg_construct e
                  id (List.length lt1) (List.length lt2)
            else
              iter_args cname l
        in
        let rec constr_type = function
          | [] -> 
              let lt2 = List.map fst lt2 in
              Error.unbound_construct e name lt2
          | (_, StrctDef _) :: ld -> constr_type ld
          | (cname, ConstrDef a) :: ld -> (
              match iter_args cname a with
              | Some name -> TDef name
              | None -> constr_type ld)
        in
        constr_type types
  in

  type_expr prog.code SymTbl.empty 
