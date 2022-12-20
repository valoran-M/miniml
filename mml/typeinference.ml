open Mml
module VSet = Set.Make (String)

type schema =
    { vars : VSet.t
    ; typ : Mml.typ
    }

module SMap = Map.Make (String)

type env = schema SMap.t

let type_inference prog =
  let new_var =
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      Printf.sprintf "tvar_%i" !cpt
  in

  let subst = Hashtbl.create 32 in

  let get_type_def name =
    snd (List.find (fun (id, _) -> name = id) prog.types)
  in

  let get_struct_with_name name =
    match get_type_def name with
    | StrctDef s -> s
    | _ -> assert false
  in

  let get_struct_args x =
    let rec aux = function
      | [] -> Mmlerror.struct_no_field x
      | (s, StrctDef arg) :: l -> (
          try
            let _, t, _ = (List.find (fun (id, _, _) -> id = x)) arg in
            (s, t)
          with Not_found -> aux l)
      | _ :: l -> aux l
    in
    aux (List.rev prog.types)
  in

  let rec unfold t =
    match t with
    | TVar s ->
        if Hashtbl.mem subst s then
          unfold (Hashtbl.find subst s)
        else
          t
    | _ -> t
  in

  let rec unfold_full t =
    match unfold t with
    | TFun (t1, t2) -> TFun (unfold_full t1, unfold_full t2)
    | _ -> t
  in

  let rec occur a t =
    match unfold_full t with
    | TVar s -> s = a
    | TFun (t1, t2) -> occur a t1 || occur a t2
    | _ -> false
  in

  let rec unify t1 t2 =
    match (unfold t1, unfold t2) with
    | TInt, TInt -> ()
    | TBool, TBool -> ()
    | TUnit, TUnit -> ()
    | TDef s1, TDef s2 when s1 = s2 -> ()
    | TFun (t1, t1'), TFun (t2, t2') ->
        unify t1 t2;
        unify t1' t2'
    | TVar a, TVar b when a = b -> ()
    | TVar a, t | t, TVar a ->
        if occur a t then
          Mmlerror.error
            (Printf.sprintf
               "unification error %s %s"
               (Mmlpp.typ_to_string t1)
               (Mmlpp.typ_to_string t2))
        else
          Hashtbl.add subst a t
    | t1, t2 ->
        Mmlerror.error
          (Printf.sprintf
             "OK unification error %s %s"
             (Mmlpp.typ_to_string t1)
             (Mmlpp.typ_to_string t2))
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

  let rec w (e : expr) env =
    match e with
    | Unit -> TUnit
    | Bool _ -> TBool
    | Uop (Not, e) ->
        let t = w e env in
        unify t TBool;
        TBool
    | Bop ((Le | Lt), e1, e2) ->
        let t1 = w e1 env in
        let t2 = w e2 env in
        unify t1 TInt;
        unify t2 TInt;
        TBool
    | Bop ((Equ   | Nequ 
          | Or    | And), e1, e2) ->
        let t = w e1 env in
        unify t (w e2 env);
        TBool
    | Bop ((Sequ | Snequ), e1, e2) ->(
        let t1 = w e1 env in 
        let t2 = w e2 env in
        unify t1 t2;
        match t1, t2 with 
        | TFun _, _ | _, TFun _ -> Mmlerror.compare_fun ()
        | _, _ -> TBool)
    | Int _ -> TInt
    | Uop (Neg, e) ->
        let t = w e env in
        unify t TInt;
        TInt
    | Bop ((Add | Sub 
          | Mod 
          | Mul | Div), e1, e2) ->
        let t1 = w e1 env in
        let t2 = w e2 env in
        unify t1 TInt;
        unify t2 TInt;
        TInt
    | Var s -> instantiate (SMap.find s env)
    | Let (s, e1, e2) ->
        let t1 = w e1 env in
        let st1 = generalize t1 env in
        let env' = SMap.add s st1 env in
        w e2 env'
    | If (c, e1, e2) ->
        unify (w c env) TBool;
        let t = w e1 env in
        unify t (w e2 env);
        t
    | Fun (x, t, e) -> (
        match t with
        | None ->
            let v = new_var () in
            let env = SMap.add x { vars = VSet.empty; typ = TVar v } env in
            let te = w e env in
            TFun (TVar v, te)
        | Some t ->
            let env = SMap.add x { vars = VSet.empty; typ = t } env in
            let te = w e env in
            TFun (t, te))
    | App (e1, e2) ->
        let t1 = w e1 env in
        let t2 = w e2 env in
        let v = TVar (new_var ()) in
        unify t1 (TFun (t2, v));
        v
    | Fix (s, t, e) -> (
        match t with
        | None ->
            let v = new_var () in
            let env = SMap.add s { vars = VSet.empty; typ = TVar v } env in
            w e env
        | Some t ->
            let env = SMap.add s { vars = VSet.empty; typ = t } env in
            w e env)
    | Seq (e1, e2) ->
        ignore (w e1 env);
        w e2 env
    | Constr (name, ex) -> construct_infer name ex env
    | Strct l -> struct_infer l env prog.types
    | GetF (e, x) -> (
        match w e env with
        | TDef s -> (
            let st = get_struct_with_name s in
            try
              let _, t, _ = (List.find (fun (id, _, _) -> id = x)) st in
              t
            with Not_found -> Mmlerror.struct_no_field x)
        | TVar _ ->
            let _, t = get_struct_args x in
            t
        | t -> Mmlerror.not_a_struct t)
    | SetF (e1, x, e2) -> (
        match w e1 env with
        | TDef s -> (
            let st = get_struct_with_name s in
            try
              let _, t, m = (List.find (fun (id, _, _) -> id = x)) st in
              unify (w e2 env) t;
              if m then
                TUnit
              else
                Mmlerror.is_not_mutable s x
            with Not_found -> Mmlerror.struct_no_field x)
        | TVar _ -> assert false
        | t -> Mmlerror.not_a_struct t)
  and struct_infer l env = function
    | [] ->
        Mmlerror.struct_construct_error
          (List.map (fun (n, e) -> (n, w e env)) l)
    | (_, ConstrDef _) :: ld -> struct_infer l env ld
    | (name, StrctDef s) :: ld -> (
        let rec iter_args = function
          | (id1, e) :: l1, (id2, t, _) :: l2 ->
              if id1 = id2 then
                try
                  unify t (w e env);
                  iter_args (l1, l2)
                with Failure _ -> None
              else
                None
          | [], [] -> Some name
          | _, _ -> None
        in
        match iter_args (l, s) with
        | Some name -> TDef name
        | None -> struct_infer l env ld)
  and construct_infer name l env =
    let lt2 = List.map (fun e -> w e env) l in
    let rec aux = function
      | [] -> Mmlerror.unbound_construct name lt2
      | (_, StrctDef _) :: ld -> aux ld
      | (cname, ConstrDef a) :: ld -> (
          let rec iter_args = function
            | [] -> None
            | (id, lt1) :: l ->
                if name = id then
                  try
                    List.iter2 (fun t1 t2 -> unify t1 t2) lt1 lt2;
                    Some cname
                  with Invalid_argument _ ->
                    Mmlerror.nb_arg_construct
                      id
                      (List.length lt1)
                      (List.length lt2)
                else
                  iter_args l
          in
          match iter_args a with
          | Some name -> TDef name
          | None -> aux ld)
    in
    aux prog.types
  in

  unfold_full (w prog.code SMap.empty)
