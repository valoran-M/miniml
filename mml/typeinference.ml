open Mml
module VSet = Set.Make (String)

type schema =
    { vars : VSet.t
    ; typ : Mml.typ
    }

module SMap = Map.Make (String)

type env = schema SMap.t

let type_inference prog file =
  let types = List.rev prog.types in

  let new_var =
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      Printf.sprintf "tvar_%i" !cpt
  in

  let subst = Hashtbl.create 32 in

  (* get type def with his name*)
  let get_type_def name =
    snd (List.find (fun (id, _) -> name = id) prog.types)
  in

  (* get struct withe her name *)
  let get_struct_with_name e name =
    match get_type_def name with
    | StrctDef s -> s
    | _ -> Error.unbound_record_field e name
  in

  (* get last struct f arg predicate *)
  let get_struct_args_fun f e x =
    let rec aux = function
      | [] -> Error.unbound_record_field e x
      | (s, StrctDef arg) :: l -> (
          try
            let _, t, _, _ = List.find f arg in
            (s, t)
          with Not_found -> aux l)
      | _ :: l -> aux l
    in
    aux types
  in

  (* get arg x in last struct find *)
  let get_struct_args e x =
    get_struct_args_fun (fun (id, _, _, _) -> id = x) e x
  in

  (* get mutable arg x in last struct find *)
  let get_struct_args_mut e x =
    get_struct_args_fun (fun (id, _, b, _) -> b && id = x) e x
  in

  (* get the type bound to t *)
  let rec unfold t =
    match t with
    | TVar s ->
        if Hashtbl.mem subst s then
          unfold (Hashtbl.find subst s)
        else
          t
    | TParam (t, s) -> TParam (unfold t, s)
    | TArray t -> TArray (unfold t)
    | _ -> t
  in

  (* get the type bound to t with rec *)
  let rec unfold_full t =
    match unfold t with
    | TFun (t1, t2) -> TFun (unfold_full t1, unfold_full t2)
    | _ -> t
  in

  (* checks if a is in t *)
  let rec occur a t =
    match unfold_full t with
    | TVar s -> s = a
    | TFun (t1, t2) -> occur a t1 || occur a t2
    | _ -> false
  in

  (* checks if two types can be unified works with constraints in the sbst
     hastable *)
  let rec unify e t1 t2 =
    match (unfold t1, unfold t2) with
    | TInt, TInt -> ()
    | TBool, TBool -> ()
    | TUnit, TUnit -> ()
    | TDef s1, TDef s2 when s1 = s2 -> ()
    | (TParam (t1', s1) as t1), (TParam (t2', s2) as t2) when s1 = s2 -> (
        try unify e t1' t2' with _ -> Error.type_error e t1 t2)
    | TFun (t1, t1'), TFun (t2, t2') ->
        unify e t1 t2;
        unify e t1' t2'
    | TVar a, TVar b when a = b -> ()
    | TArray a, TArray b -> unify e a b
    | TVar a, t | t, TVar a ->
        if occur a t then
          Error.type_error e (unfold_full t1) (unfold_full t2)
        else
          Hashtbl.add subst a t
    | t1, t2 -> Error.type_error e (unfold_full t1) (unfold_full t2)
  in

  let instantiate s =
    let renaming =
      VSet.fold (fun v r -> SMap.add v (TVar (new_var ())) r) s.vars SMap.empty
    in

    let rec rename t =
      match unfold t with
      | TVar a as t -> ( try SMap.find a renaming with Not_found -> t)
      | TFun (t1, t2) -> TFun (rename t1, rename t2)
      | _ -> t
    in
    rename s.typ
  in

  let rec fvars t =
    match unfold t with
    | TFun (t1, t2) -> VSet.union (fvars t1) (fvars t2)
    | TVar x -> VSet.singleton x
    | _ -> VSet.empty
  in

  let schema_fvars s = VSet.diff (fvars s.typ) s.vars in

  let generalize t env =
    let fvt = fvars t in
    let fvenv =
      SMap.fold (fun _ s vs -> VSet.union (schema_fvars s) vs) env VSet.empty
    in
    { vars = VSet.diff fvt fvenv; typ = t }
  in

  let rec w (e : expr_loc) env =
    match e.expr with
    | Unit -> TUnit
    | Bool _ -> TBool
    | Uop (Not, e) ->
        let t = w e env in
        unify e t TBool;
        TBool
    | Bop ((Le | Lt | Gr | Gre), e1, e2) ->
        let t1 = w e1 env in
        let t2 = w e2 env in
        unify e1 t1 TInt;
        unify e2 t2 TInt;
        TBool
    | Bop ((Equ | Nequ | Sequ | Snequ), e1, e2) ->
        let t = w e1 env in
        unify e2 (w e2 env) t;
        TBool
    | Bop ((Or | And), e1, e2) ->
        let t1 = w e1 env in
        unify e1 t1 TBool;
        let t2 = w e2 env in
        unify e2 t2 TBool;
        TBool
    | Int _ -> TInt
    | Uop (Neg, e) ->
        let t = w e env in
        unify e t TInt;
        TInt
    | Bop ((Add | Sub | Mod | Mul | Div), e1, e2) ->
        let t1 = w e1 env in
        let t2 = w e2 env in
        unify e1 t1 TInt;
        unify e2 t2 TInt;
        TInt
    | Var s -> (
        try instantiate (SMap.find s env)
        with Not_found -> Error.unbound_value e s (SMap.fold (fun s _ acc -> s ::acc) env []))
    | Let (s, e1, None, e2) ->
        let t1 = w e1 env in
        let st1 = generalize t1 env in
        let env' = SMap.add s st1 env in
        w e2 env'
    | Let (s, e1, Some t, e2) ->
        let t1 = w e1 env in
        unify e1 t1 t;
        let st1 = generalize t1 env in
        let env' = SMap.add s st1 env in
        w e2 env'
    | If (c, e1, Some e2) ->
        unify e1 (w c env) TBool;
        let t = w e1 env in
        unify e2 (w e2 env) t;
        t
    | If (c, e, None) ->
        unify c (w c env) TBool;
        let t = w e env in
        unify e t TUnit;
        t
    | Fun (x, None, e) ->
        let v = new_var () in
        let env = SMap.add x { vars = VSet.empty; typ = TVar v } env in
        let te = w e env in
        TFun (TVar v, te)
    | Fun (x, Some t, e) ->
        let env = SMap.add x { vars = VSet.empty; typ = t } env in
        let te = w e env in
        TFun (t, te)
    | App (e1, e2) -> app_infer e e1 e2 env
    | Fix (s, None, e) ->
        let v = new_var () in
        let env = SMap.add s { vars = VSet.empty; typ = TVar v } env in
        w e env
    | Fix (s, Some t, e) ->
        let env = SMap.add s { vars = VSet.empty; typ = t } env in
        w e env
    | Seq (e1, e2) ->
        if unfold (w e1 env) <> TUnit then Errorcat.warn_not_unit file e1;
        w e2 env
    | Constr (name, ex) -> construct_infer e name ex env
    | Strct l -> struct_infer e l env
    | GetF (se, x) -> getf_infer e se x env
    | SetF (e1, x, e2) -> setf_infer e e1 x e2 env
    | Array l ->
        let var = TVar (new_var ()) in
        List.iter (fun e -> unify e (w e env) var) l;
        TArray var
    | NArray (e, n) ->
        unify n (w n env) TInt;
        TArray (w e env)
    | GetI (e, i) ->
        let var = TVar (new_var ()) in
        unify e (w e env) (TArray var);
        unify i (w i env) TInt;
        var
    | SetI (e1, i, e2) ->
        let var = TVar (new_var ()) in
        unify e1 (w e1 env) (TArray var);
        unify i (w i env) TInt;
        unify e2 (w e2 env) var;
        TUnit
    | Match (_, _) -> assert false
  and app_infer e e1 e2 env =
    let t = w e1 env in
    match t with
    | TFun (t1, t1') ->
        let t2 = w e2 env in
        unify e2 t2 t1;
        t1'
    | TVar _ ->
        let t1 = w e2 env in
        let t2 = new_var () in
        unify e t (TFun (t1, TVar t2));
        TVar t2
    | t -> Error.not_a_function e1 t
  (* retrieve the structure with the ids and the types so two iterations to do
     (typdef then in the defined structure)*)
  and struct_infer e l env =
    let f s =
      let iter_args (id1, e) (id2, t, _, _) =
        if id1 = id2 then
          try
            unify e t (w e env);
            true
          with _ -> false
        else
          false
      in
      try List.for_all2 iter_args l s with _ -> false
    in
    try TDef (Iter.struct_find f types)
    with _ ->
      Error.struct_construct_error e (List.map (fun (n, e) -> (n, w e env)) l)
  and getf_infer e se x env =
    match w se env with
    | TDef s -> (
        let st = get_struct_with_name se s in
        try
          let _, t, _, _ = (List.find (fun (id, _, _, _) -> id = x)) st in
          t
        with Not_found -> Error.struct_no_field e s x)
    | TVar _ as t1 ->
        let t2 = snd (get_struct_args e x) in
        unify e t1 t2;
        t2
    | t -> Error.not_a_struct e t
  and setf_infer e e1 x e2 env =
    match w e1 env with
    | TDef s -> (
        let st = get_struct_with_name e s in
        try
          let _, t, m, _ = (List.find (fun (id, _, _, _) -> id = x)) st in
          unify e2 (w e2 env) t;
          if m then
            TUnit
          else
            Error.is_not_mutable e s x
        with Not_found -> Error.struct_no_field e s x)
    | TVar _ as t1 ->
        let t2 = snd (get_struct_args_mut e x) in
        unify e t1 t2;
        t2
    | t -> Error.not_a_struct e1 t
  and construct_infer e name l env =
    let lt2 = List.map (fun e -> (w e env, e)) l in
    let rec iter_args cname = function
      | [] -> None
      | (id, lt1) :: l ->
          if name = id then
            try
              List.iter2 (fun (t1, _) (t2, loc) -> unify loc t2 t1) lt1 lt2;
              Some cname
            with Invalid_argument _ ->
              Error.nb_arg_construct e id (List.length lt1) (List.length lt2)
          else
            iter_args cname l
    in
    let rec aux = function
      | [] ->
          let lt2 = List.map fst lt2 in
          Error.unbound_construct e name lt2
      | (_, StrctDef _) :: ld -> aux ld
      | (cname, ConstrDef (a, t)) :: ld -> (
          match iter_args cname a with
          | Some name -> constr_match name t
          | None -> aux ld)
    and constr_match name = function
      | Some t ->
          let tparam = unfold (TVar t) in
          Hashtbl.remove subst t;
          TParam (tparam, name)
      | None -> TDef name
    in
    aux types
  in

  Constructdef.verif_construct prog.types;
  unfold_full (w prog.code SMap.empty)
