open Mml
module VSet = Set.Make (String)

type schema = {
    vars : VSet.t;
    typ : Mml.typ;
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
    | TStrct s1, TStrct s2 when s1 = s2 -> ()
    | TFun (t1, t1'), TFun (t2, t2') ->
        unify t1 t2;
        unify t1' t2'
    | TVar a, TVar b when a = b -> ()
    | TVar a, t | t, TVar a ->
        if occur a t then
          failwith
            (Printf.sprintf "unification error %s %s" (Mmlpp.typ_to_string t1)
               (Mmlpp.typ_to_string t2))
        else
          Hashtbl.add subst a t
    | _, _ ->
        failwith
          (Printf.sprintf "unification error %s %s" (Mmlpp.typ_to_string t1)
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
    | Bop ((Equ | Nequ | Or | And), e1, e2) ->
        let t = w e1 env in
        unify t (w e2 env);
        TBool
    | Int _ -> TInt
    | Uop (Neg, e) ->
        let t = w e env in
        unify t TInt;
        TInt
    | Bop ((Add | Sub | Mod | Mul | Div), e1, e2) ->
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
    | Fun (x, _, e) ->
        let v = new_var () in
        let env = SMap.add x { vars = VSet.empty; typ = TVar v } env in
        let t = w e env in
        TFun (TVar v, t)
    | App (e1, e2) ->
        let t1 = w e1 env in
        let t2 = w e2 env in
        let v = TVar (new_var ()) in
        unify t1 (TFun (t2, v));
        v
    | Fix (s, _, e) ->
        let v = new_var () in 
        let env = SMap.add s { vars = VSet.empty; typ = TVar v} env in 
        w e env
    | Seq (e1, e2) ->
        ignore (w e1 env);
        w e2 env
    | _ -> assert false
  in

  unfold_full (w prog.code SMap.empty)
