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
        else t
    | TRef t -> TRef (unfold t)
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
  let rec unify l t1 t2 =
    match (unfold t1, unfold t2) with
    | TInt, TInt -> ()
    | TBool, TBool -> ()
    | TChar, TChar -> ()
    | TString, TString -> ()
    | TUnit, TUnit -> ()
    | TRef t1, TRef t2 ->
        unify l t1 t2
    | TDef s1, TDef s2 when s1 = s2 -> ()
    | (TParam (t1', s1) as t1), (TParam (t2', s2) as t2) when s1 = s2 -> (
        try unify l t1' t2' with _ -> Error.type_error l t1 t2)
    | TFun (t1, t1'), TFun (t2, t2') ->
        unify l t1 t2;
        unify l t1' t2'
    | TVar a, TVar b when a = b -> ()
    | TArray a, TArray b -> unify l a b
    | TVar a, t | t, TVar a ->
        if occur a t then
          Error.type_error l (unfold_full t1) (unfold_full t2)
        else
          Hashtbl.add subst a t
    | t1, t2 -> Error.type_error l (unfold_full t1) (unfold_full t2)
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

  (* pattern *)

  (* return type pattern with env *)
  let rec type_pattern p (env: env) =
    match p.pat with
    | Pat_jok     -> (TVar (new_var ()), env)
    | Pat_var x   ->
        let v = new_var () in
        let env = SMap.add x { vars = VSet.empty; typ = TVar v } env in
        (TVar (new_var ()), env)
    | Pat_int _             -> (TInt, env)
    | Pat_bool _            -> (TBool, env)
    | Pat_construct (c, l)  -> (
        let f name (lc : (string * (typ * location) list) list)
            (s : string option) env =
          let fc (name, (types : (typ * location) list)) env =
            if c <> name then
              (false, env)
            else
              ( true, List.fold_left2
                  (fun env (t, _) (p : pattern_loc) ->
                    let tp, env = type_pattern p env in
                    unify p.loc t tp;
                    env) env types l )
          in
          let b, env = Iter.list_find_ret fc lc env in
          (b, (name, s, env))
        in
        try
          let n, opt, env = Iter.construct_find_ret f types env in
          match opt with
          | None    -> (TDef n, env)
          | Some s  ->
              let t = unfold_full (TVar s) in
              Hashtbl.remove subst s;
              (TParam (t, n), env)
        with Not_found -> Error.unbound_construct p.loc c [])
  in

  let rec w (e : expr_loc) env =
    match e.expr with
    | Unit      -> TUnit
    | Bool _    -> TBool
    | Char _    -> TChar
    | String _  -> TString
    | Ref e     -> TRef (w e env)
    | SetRef ((id, loc), e) ->
        unify loc (SMap.find id env).typ (TRef (w e env));
        TUnit
    | Uop(GetRef, e) ->
        let t = TVar(new_var ()) in
        unify e.loc (w e env) (TRef(t));
        unfold t
    | Uop (Slength, e) -> 
        unify e.loc (w e env) TString; 
        TInt
    | GetS (e, i) ->
        unify e.loc (w e env) TString;
        unify i.loc (w i env) TInt;
        TChar
    | Uop (Not, e) ->
        let t = w e env in
        unify e.loc t TBool;
        TBool
    | Bop (Concat, e1, e2) ->
        unify e1.loc (w e1 env) TString;
        unify e2.loc (w e2 env) TString;
        TString
    | Bop ((Le | Lt | Gr | Gre), e1, e2) ->
        let t1 = w e1 env in
        let t2 = w e2 env in
        unify e1.loc t1 TInt;
        unify e2.loc t2 TInt;
        TBool
    | Bop ((Equ | Nequ | Sequ | Snequ), e1, e2) ->
        let t = w e1 env in
        unify e2.loc (w e2 env) t;
        TBool
    | Bop ((Or | And), e1, e2) ->
        let t1 = w e1 env in
        unify e1.loc t1 TBool;
        let t2 = w e2 env in
        unify e2.loc t2 TBool;
        TBool
    | Int _ -> TInt
    | Uop (Neg, e) ->
        let t = w e env in
        unify e.loc t TInt;
        TInt
    | Bop ((Add | Sub | Mod | Mul | Div), e1, e2) ->
        let t1 = w e1 env in
        let t2 = w e2 env in
        unify e1.loc t1 TInt;
        unify e2.loc t2 TInt;
        TInt
    | Var s -> (
        try instantiate (SMap.find s env)
        with Not_found ->
          Error.unbound_value
            e.loc
            s
            (SMap.fold (fun s _ acc -> s :: acc) env []))
    | Let (s, e1, None, e2) ->
        let t1 = w e1 env in
        let st1 = generalize t1 env in
        let env' = SMap.add s st1 env in
        w e2 env'
    | Let (s, e1, Some t, e2) ->
        let t1 = w e1 env in
        unify e1.loc t1 t;
        let st1 = generalize t1 env in
        let env' = SMap.add s st1 env in
        w e2 env'
    | If (c, e1, Some e2) ->
        unify e1.loc (w c env) TBool;
        let t = w e1 env in
        unify e2.loc (w e2 env) t;
        t
    | If (c, e, None) ->
        unify c.loc (w c env) TBool;
        let t = w e env in
        unify e.loc t TUnit;
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
        List.iter (fun e -> unify e.loc (w e env) var) l;
        TArray var
    | NArray (e, n) ->
        unify n.loc (w n env) TInt;
        TArray (w e env)
    | Uop (Alength, e) ->
        unify e.loc (w e env) (TArray(TVar (new_var ())));
        TInt
    | GetI (e, i) ->
        let var = TVar (new_var ()) in
        unify e.loc (w e env) (TArray var);
        unify i.loc (w i env) TInt;
        var
    | SetI (e1, i, e2) ->
        let var = TVar (new_var ()) in
        unify e1.loc (w e1 env) (TArray var);
        unify i.loc (w i env) TInt;
        unify e2.loc (w e2 env) var;
        TUnit
    | Match (e, lp) ->
        let t = w e env in
        let tm = TVar (new_var ()) in
        List.iter
          (fun ((p : pattern_loc), e) ->
            let tp, env = type_pattern p env in
            unify p.loc tp t;
            unify e.loc tm (w e env))
          lp;
        unfold tm
    | Print (t, e) -> check_print t e env
  and app_infer e e1 e2 env =
    let t = w e1 env in
    match t with
    | TFun (t1, t1') ->
        let t2 = w e2 env in
        unify e2.loc t2 t1;
        t1'
    | TVar _ ->
        let t1 = w e2 env in
        let t2 = new_var () in
        unify e.loc t (TFun (t1, TVar t2));
        TVar t2
    | t -> Error.not_a_function e1.loc t
  (* retrieve the structure with the ids and the types so two iterations to do
     (typdef then in the defined structure)*)
  and struct_infer e l env =
    let iter_args (id1, e) (id2, t, _, _) =
      if id1 = id2 then
        try unify e.loc t (w e env); true
        with _ -> false
      else
        false
    in
    try
      TDef
        (Iter.struct_find
           (fun s -> try List.for_all2 iter_args l s with _ -> false)
           types)
    with _ ->
      Error.struct_construct_error
        e.loc
        (List.map (fun (n, e) -> (n, w e env)) l)
  and getf_infer e se x env =
    match w se env with
    | TDef s -> (
        let st = get_struct_with_name se.loc s in
        try
          let _, t, _, _ = (List.find (fun (id, _, _, _) -> id = x)) st in
          t
        with Not_found -> Error.struct_no_field e.loc s x)
    | TVar _ as t1 ->
        let t2 = snd (get_struct_args e.loc x) in
        unify e.loc t1 t2;
        t2
    | t -> Error.not_a_struct e.loc t
  and setf_infer e e1 x e2 env =
    match w e1 env with
    | TDef s -> (
        let st = get_struct_with_name e.loc s in
        try
          let _, t, m, _ = (List.find (fun (id, _, _, _) -> id = x)) st in
          unify e2.loc (w e2 env) t;
          if m  then TUnit
                else Error.is_not_mutable e.loc s x
        with Not_found -> Error.struct_no_field e.loc s x)
    | TVar _ as t1 ->
        let t2 = snd (get_struct_args_mut e.loc x) in
        unify e.loc t1 t2;
        t2
    | t -> Error.not_a_struct e1.loc t
  and construct_infer e name l env =
    let lt2 = List.map (fun e -> (w e env, e)) l in
    let rec iter_args cname lc s acc =
      match lc with
      | [] -> (false, acc)
      | (id, lt1) :: l ->
          if name = id then
            try
              List.iter2 (fun (t1, _) (t2, e) -> unify e.loc t2 t1) lt1 lt2;
              (true, (cname, s))
            with Invalid_argument _ ->
              Error.nb_arg_construct
                e.loc id (List.length lt1) (List.length lt2)
          else
            iter_args cname l s acc
    in
    let constr_type (name, tparam) =
      match tparam with
      | Some t ->
          let tparam = unfold (TVar t) in
          Hashtbl.remove subst t;
          TParam (tparam, name)
      | None -> TDef name
    in
    constr_type (Iter.construct_find_ret iter_args types ("", None))
  and check_print t e env =
    (match t with
    | Pt_int -> unify e.loc TInt (w e env)
    | Pt_bool -> unify e.loc TBool (w e env)
    | Pt_newline -> unify e.loc TUnit (w e env)
    | Pt_char -> unify e.loc TChar (w e env)
    | Pt_string | Pt_endline -> unify e.loc TString (w e env));
    TUnit
  in

  Constructdef.verif_construct prog.types;
  unfold_full (w prog.code SMap.empty)
