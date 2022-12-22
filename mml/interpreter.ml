open Mml

(* Environnement : associe des valeurs à des noms de variables *)
module Env = Map.Make (String)

(* Valeurs *)
type value =
    | VInt  of int
    | VBool of bool
    | VUnit
    | VPtr  of int

(* Élements du tas *)
type heap_value =
    | VClos   of string * expr_loc * value Env.t
    | VStrct  of (string, value) Hashtbl.t
    | VConstr of string * value list 

(* Affiche les valeurs retourner par eval_prog *)
let rec print_value mem = function
  | VInt n  -> Printf.printf "%d" n
  | VBool b -> Printf.printf "%b" b
  | VUnit   -> Printf.printf "()"
  | VPtr p  -> 
      Printf.printf "@%d ->" p; 
      print_value_in_mem p mem

and print_value_in_mem p mem =
  match Hashtbl.find mem p with 
  | VClos (s, _, _) -> Printf.printf "%s" s 
  | VStrct s -> print_struct mem s
  | VConstr (s, v) -> 
      Printf.printf "%s (" s;
      print_list_value mem v

and print_struct mem s =
  Printf.printf "{ ";
  Hashtbl.iter (fun id v -> Printf.printf "%s = " id; 
                            print_value mem v; 
                            Printf.printf "; ") s;
  print_char '}'

and print_list_value mem = function
  | []      -> ()
  | [v]     -> 
      print_value mem v; 
      print_char ')'
  | v :: l  -> 
      print_value mem v; 
      print_list_value mem l

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

  (* rend une strucutre ou si elle n'existe pas 
      déclanche une erreur *)
  let find_struct a =
    match Hashtbl.find mem a with
    | VStrct s -> s
    | _ -> assert false
  in

  (* Interprétation d'une expression, en fonction d'un environnement
      et de la mémoire globale *)
  let rec eval (e : expr_loc) (env : value Env.t) : value =
    match e.expr with
    | Unit            -> VUnit
    | Var id          -> Env.find id env
    (* Opération Booléenne *)
    | Bool b          -> VBool b
    | (Uop (Not, _) 
    | Bop ((Equ   | Nequ 
          | Sequ  | Snequ
          | Le    | Lt 
          | Or    | And), _, _))    -> VBool (evalb e env)
    (* Opération Arithmétique *)
    | Int n           -> VInt n
    | (Uop (Neg, _) 
    | Bop ((Add | Sub 
          | Mod 
          | Mul | Div), _, _))      -> VInt (evali e env)
    (* Fonction *)
    | Fun (id, _, e)      -> eval_fun id e env
    | Let (id, e1, _, e2) -> eval e2 (Env.add id (eval e1 env) env)
    | App (e1, e2)        -> eval_app e1 e2 env
    | Fix (id, _, e)      -> eval_fix id e env
    (* struct *)
    | Strct s             -> create_struct s env
    | GetF (e, id)        -> eval_getf e id env
    | SetF (e1, id, e2)   -> eval_setf e1 id e2 env
    (* Constr *)
    | Constr (s, l)       -> create_const s l env
    (* Autres *)
    | If (c, e1, Some e2) -> if evalb c env then eval e1 env 
                                            else eval e2 env
    | If (c, e, None)     -> if evalb c env then eval e env
                                            else VUnit
    | Seq (e1, e2)        -> let _ = eval e1 env in eval e2 env

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
    | Bop (Sequ, e1, e2)  -> struc_equal e  (eval e1 env) 
                                            (eval e2 env)
    | Bop (Snequ, e1, e2) -> not (struc_equal e (eval e1 env) 
                                                (eval e2 env))
    | Bop (Le, e1, e2)    -> evali e1 env <= evali e2 env
    | Bop (Lt, e1, e2)    -> evali e1 env < evali e2 env
    | Bop (Or, e1, e2)    -> evalb e1 env || evalb e2 env
    | Bop (And, e1, e2)   -> evalb e1 env && evalb e2 env
    | _ -> (
        match eval e env with
        | VBool b -> b
        | _ -> assert false)
  and struc_equal e (v1: value) (v2: value) : bool =
    match v1, v2 with
    | VInt  v1  , VInt  v2  -> v1 = v2
    | VBool b1  , VBool b2  -> b1 = b2
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
  (* eval fun id -> e *)
  and eval_fun (id: string) (e: expr_loc) env :value =
    let ptr = new_ptr () in
    Hashtbl.add mem ptr (VClos (id, e, env));
    VPtr ptr
  (* eval e1 e2 *)
  and eval_app (e1: expr_loc) (e2: expr_loc) env :value=
    let val_e2 = eval e2 env in
    match eval e1 env with
    | VPtr p -> (
        match Hashtbl.find mem p with
        | VClos (id, e, env) -> eval e (Env.add id val_e2 env)
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
        let struct_content = Hashtbl.create (List.length struct_list) in
        Hashtbl.add mem ptr (VStrct struct_content);
        List.iter (fun (field, valeur) -> 
          (Hashtbl.add struct_content field 
              (eval valeur (Env.add id (VPtr ptr) env)))) 
          struct_list;
        VPtr ptr
    | _ -> assert false
  (* eval e2.id <- e2 *)
  and eval_setf (e1: expr_loc) (id: string) (e2: expr_loc) env : value =
    let new_val = eval e2 env in
    match eval e1 env with
    | VPtr a ->
        let s = find_struct a in
        Hashtbl.replace s id new_val;
        VUnit
    | _ -> assert false
  (* eval e.id *)
  and eval_getf (e: expr_loc) (id: string) env : value =
    match eval e env with
    | VPtr a -> Hashtbl.find (find_struct a) id
    | _ -> assert false
  (* eval {a1 = e1; ... an = en; } *)
  and create_struct (s : (string * expr_loc) list) env : value =
    let data = Hashtbl.create (List.length s) in
    List.iter (fun (id, valeur) -> Hashtbl.add data id (eval valeur env)) s;
    let ptr = new_ptr () in
    Hashtbl.add mem ptr (VStrct data);
    VPtr ptr
  and create_const (s: string) (l: expr_loc list) env :value =
    let rec aux = function
      | [] -> []
      | e::l -> eval e env :: aux l
    in
    let ptr = new_ptr () in
    Hashtbl.add mem ptr (VConstr (s, aux l));
    VPtr ptr
  in

  (eval prog.code Env.empty, mem)
