open Mml
open Value

(* Interprète un programme *)
let eval_prog (prog : prog) : value * (int, heap_value) Hashtbl.t =
  (* Initialisation de la mémoire globale *)
  let (mem : (int, heap_value) Hashtbl.t) = Hashtbl.create 16 in

  (* Création de nouvelles adresses *)
  let new_ptr =
    let cpt = ref 0 in
    fun () ->
      incr cpt;
      !cpt
  in

  (* rend une strucutre si elle n'existe pas 
      déclanche une erreur *)
  let find_struct a =
    match Hashtbl.find mem a with
    | VStrct s -> s
    | _ -> assert false
  in

  let rec struc_equal e (v1: value) (v2: value) : bool =
    match v1, v2 with
    | VInt  v1  , VInt  v2  -> v1 = v2
    | VBool b1  , VBool b2  -> b1 = b2
    | VChar c1  , VChar c2  -> c1 = c2
    | VString s1, VString s2-> s1 = s2
    | VPtr  pt1 , VPtr  pt2 -> (
        match (Hashtbl.find mem pt1, Hashtbl.find mem pt2) with 
        | VClos _, _ | _, VClos _ -> 
            Error.compare_fun e;
        | VStrct s1, VStrct s2 -> 
            Hashtbl.fold 
              (fun id v acc -> acc && struc_equal e v (Hashtbl.find s2 id)) 
              s1 true
        | VConstr (s1, c1), VConstr (s2, c2) when s1 = s2 -> 
            List.fold_left2 
              (fun acc v1 v2 -> acc && struc_equal e v1 v2) 
              true c1 c2
        | _, _ -> false) 
    | _, _ -> false
  in 

  let set_index (v1: value) (i: int) (v2:value) : value =
    let ptr = (match v1 with | VPtr ptr -> ptr | _ -> assert false) in
    match Hashtbl.find mem ptr with
    | VArray a ->(
        try a.(i) <- (v2); VUnit
        with Invalid_argument s -> 
          raise (Error.Error (Error.Invalid_argument s)))
    | _ -> assert false
  in

  (* eval fun id -> e *)
  let eval_fun (id: string) (e: expr_loc) env :value =
    let ptr = new_ptr () in
    Hashtbl.add mem ptr (VClos (id, e, env));
    VPtr ptr
  in

  (* eval e2.id <- e2 *)
  let eval_setf (v1: value) (id: string) (v2: value) : value =
    match v1 with
    | VPtr a ->
        let s = find_struct a in
        Hashtbl.replace s id v2;
        VUnit
    | _ -> assert false
  in

  (* eval e.id *)
  let eval_getf (v: value) (id: string) : value =
    match v with
    | VPtr a -> Hashtbl.find (find_struct a) id
    | _ -> assert false
  in

  (* eval e.(i) *)
  let get_index (v: value) (i: int): value =
    let ptr = (match v with | VPtr ptr -> ptr | _ -> assert false) in
    match Hashtbl.find mem ptr with
    | VArray a ->(
        try a.(i)
        with Invalid_argument s -> 
          raise (Error.Error (Error.Invalid_argument s)))
    | _ -> assert false
  in

  let create_narray (v:value) (n:int): value =
    let ptr = new_ptr () in
    Hashtbl.add mem ptr (VArray (Array.make n v));
    VPtr ptr
  in

  let get_char_string (v: value) (i: int): value = 
    match v with
    | VString s ->(
        try VChar s.[i]
        with Invalid_argument s -> 
          raise (Error.Error (Error.Invalid_argument s)))
    | _ -> assert false
  in

  let concat_string (v1: value) (v2: value): value = 
    match v1, v2 with 
    | VString s1, VString s2 -> VString (s1 ^ s2)
    | _, _ -> assert false
  in

  let eval_print t v = 
    match t with 
    | Pt_int | Pt_bool -> print_string (Value.value_to_string mem v)
    | Pt_char          -> print_string(Value.value_to_string mem v)
    | Pt_string        -> print_string (Value.value_to_string mem v) 
    | Pt_endline       -> print_endline (Value.value_to_string mem v)
    | Pt_newline       -> print_newline ();
  in

  (* Interprétation d'une expression, en fonction d'un environnement
      et de la mémoire globale *)
  let rec eval (e : expr_loc) (env : value Env.t) : value =
    match e.expr with
    | Unit            -> VUnit
    | Var id          -> Env.find id env
    (* char string *)
    | Char c          -> VChar c
    | String s        -> VString s
    | GetS (e, i)     -> get_char_string (eval e env) (evali i env)
    | Bop (Concat, 
          e1, e2)     -> concat_string (eval e1 env) (eval e2 env)
    (* Opération Booléenne *)
    | Bool b          -> VBool b
    | (Uop (Not, _) 
    | Bop ((Equ   | Nequ 
          | Sequ  | Snequ
          | Le    | Lt 
          | Gr    | Gre
          | Or    | And), _, _)) -> VBool (evalb e env)
    (* Opération Arithmétique *)
    | Int n           -> VInt n
    | (Uop (Neg, _) 
    | Bop ((Add | Sub 
          | Mod 
          | Mul | Div), _, _)) -> VInt (evali e env)
    (* Fonction *)
    | Fun (id, _, e)      -> eval_fun id e env
    | Let (id, e1, _, e2) -> eval e2 (Env.add id (eval e1 env) env)
    | App (e1, e2)        -> eval_app (eval e1 env) (eval e2 env)
    | Fix (id, _, e)      -> eval_fix id e env
    (* struct *)
    | Strct s             -> create_struct s env
    | GetF (e, id)        -> eval_getf (eval e env) id
    | SetF (e1, id, e2)   -> eval_setf (eval e1 env) id (eval e2 env)
    (* Constr *)
    | Constr (s, l)       -> create_const s l env
    (* Autres *)
    | If (c, e1, Some e2) -> if evalb c env then eval e1 env 
                                            else eval e2 env
    | If (c, e, None)     -> if evalb c env then eval e env
                                            else VUnit
    | Seq (e1, e2)        -> let _ = eval e1 env in eval e2 env
    | Array l             -> create_array l env
    | NArray (e, n)       -> create_narray (eval e env) (evali n env)
    | GetI (e, i)         -> get_index (eval e env) (evali i env)
    | SetI (e1, i, e2)    -> set_index (eval e1 env) (evali i env) (eval e2 env)
    | Match (m, l)        -> eval_match (eval m env) l e.loc env;
    | Print (t, e)        -> eval_print t (eval e env); VUnit
        

  (* Évaluation d'une expression dont la valeur est supposée entière *)
  and evali (e : expr_loc) (env : value Env.t) : int =
    match e.expr with
    | Int n             -> n
    | Uop (Neg, e)      -> -evali e env
    | Bop (Add, e1, e2) -> evali e1 env + evali e2 env
    | Bop (Sub, e1, e2) -> evali e1 env - evali e2 env
    | Bop (Mod, e1, e2) -> evali e1 env mod evali e2 env
    | Bop (Mul, e1, e2) -> evali e1 env * evali e2 env
    | Bop (Div, e1, e2) -> evali e1 env / evali e2 env
    | _ -> (
        match eval e env with
        | VInt n -> n
        | _ -> assert false)
  (* Évaluation d'une expression dont la valeur est supposée booléenne *)
  and evalb (e : expr_loc) (env : value Env.t) : bool =
    match e.expr with
    | Bool b              -> b
    | Uop (Not, e)        -> not (evalb e env)
    | Bop (Equ, e1, e2)   -> eval e1 env = eval e2 env
    | Bop (Nequ, e1, e2)  -> evali e1 env != evali e2 env
    | Bop (Sequ, e1, e2)  -> struc_equal e.loc  (eval e1 env) 
                                                (eval e2 env)
    | Bop (Snequ, e1, e2) -> not (struc_equal e.loc (eval e1 env) 
                                                    (eval e2 env))
    | Bop (Le, e1, e2)    -> evali e1 env <= evali e2 env
    | Bop (Lt, e1, e2)    -> evali e1 env < evali e2 env
    | Bop (Gre, e1, e2)   -> evali e1 env >= evali e2 env
    | Bop (Gr, e1, e2)    -> evali e1 env > evali e2 env

    | Bop (Or, e1, e2)    -> evalb e1 env || evalb e2 env
    | Bop (And, e1, e2)   -> evalb e1 env && evalb e2 env
    | _ -> (
        match eval e env with
        | VBool b -> b
        | _ -> assert false)
  (* eval e1 e2 *)
  and eval_app (v1: value) (v2: value) :value=
    match v1 with
    | VPtr p -> (
        match Hashtbl.find mem p with
        | VClos (id, e, env) -> eval e (Env.add id v2 env)
        | _ -> assert false)
    | _ -> assert false
  (* eval Fix(id, t, e) *)
  and eval_fix (id: string) (e: expr_loc) env :value=
    match e.expr with
    | Fun(f, _, e) -> 
        let ptr = new_ptr () in
        Hashtbl.add mem ptr (VClos (f, e, Env.add id (VPtr ptr) env));
        VPtr ptr
    | Strct struct_list ->
        let ptr = new_ptr () in
        let env  = Env.add id (VPtr ptr) env in
        let struct_content = Hashtbl.create (List.length struct_list) in
        Hashtbl.add mem ptr (VStrct struct_content);
        List.iter (fun (field, valeur) -> 
          (Hashtbl.add struct_content field (eval valeur env))) 
          struct_list;
        VPtr ptr
    | Constr(s, l) -> 
        let ptr = new_ptr () in
        let env  = Env.add id (VPtr ptr) env in
        Hashtbl.add mem ptr (VConstr (s, List.map (fun e -> eval e env) l));
        VPtr ptr
    | _ -> assert false
  (* eval {a1 = e1; ... an = en; } *)
  and create_struct (s : (string * expr_loc) list) env : value =
    let data = Hashtbl.create (List.length s) in
    List.iter (fun (id, valeur) -> Hashtbl.add data id (eval valeur env)) s;
    let ptr = new_ptr () in
    Hashtbl.add mem ptr (VStrct data);
    VPtr ptr
  and create_const (s: string) (l: expr_loc list) env :value =
    let ptr = new_ptr () in
    Hashtbl.add mem ptr (VConstr (s, List.map (fun e -> eval e env) l));
    VPtr ptr
  and create_array (l: expr_loc list) env: value=
    let ptr = new_ptr () in
    Hashtbl.add mem ptr 
      (VArray (Array.of_list (List.map (fun e -> eval e env) l)));
    VPtr ptr
  and eval_match (v: value) (l:(pattern_loc * expr_loc) list) loc env :value= 
    try
      let p, e = List.find (Patern.unify_list_expr_pattern v mem) l in
      eval e (Patern.set_env_for_expr p.pat v mem env)
    with Not_found -> Error.raise_match_failure loc mem v
  in 

  (eval prog.code Env.empty, mem)
