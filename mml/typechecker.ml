open Mml

(* Environnement de typage : associe des types aux noms de variables *)
module SymTbl = Map.Make (String)

type tenv = typ SymTbl.t

(* Pour remonter des erreurs circonstanciées *)
exception Type_error of string

let error s = raise (Type_error s)

let type_error ty_actual ty_expected =
  error
    (Printf.sprintf "expected %s but got %s"
       (Mmlpp.typ_to_string ty_expected)
       (Mmlpp.typ_to_string ty_actual))
(* vous pouvez ajouter d'autres types d'erreurs *)

let struct_construct_error l =
  let rec print_struct = function
    | [] -> ""
    | (n, t) :: l ->
        Format.sprintf "%s: %s ; %s" n (Mmlpp.typ_to_string t) (print_struct l)
  in
  error
    (Printf.sprintf "The structure of the form {%s} does not exist"
       (print_struct l))

(* Vérification des types d'un programme *)
let type_prog prog =
  let get_type_def name =
    snd (List.find (fun (id, _) -> name = id) prog.types)
  in

  let get_struct name =
    match get_type_def name with
    | StrctDef s -> s
    | _ -> assert false
  in

  (* let get_enum name = *)
  (*   match get_type_def name with *)
  (*   | EnumDef e -> e *)
  (*   | _ -> assert false *)
  (* in *)

  (* Vérifie que l'expression [e] a le type [type] *)
  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ
  (* Calcule le type de l'expression [e] *)
  and type_expr e tenv =
    match e with
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
        check e1 TInt tenv;
        check e2 TInt tenv;
        TBool
    | Bop ((Equ | Nequ
            |Lt | Le ), e1, e2) ->
        check e1 (type_expr e2 tenv) tenv;
        TBool
    | Var s -> SymTbl.find s tenv
    | Let (s, e1, e2) ->
        let t1 = type_expr e1 tenv in
        type_expr e2 (SymTbl.add s t1 tenv)
    | If (c, e1, e2) ->
        check c TBool tenv;
        let t2 = type_expr e2 tenv in
        check e1 t2 tenv;
        t2
    | Fun (id, t, e) -> TFun (t, type_expr e (SymTbl.add id t tenv))
    | App (e1, e2) -> (
        match type_expr e1 tenv with
        | TFun (t1, t2) ->
            check e2 t1 tenv;
            t2
        | t ->
            error
              (Printf.sprintf
                 "\n\
                  This expression has typ %s\n\
                  This is not a function; it cannot be applied.\n"
                 (Mmlpp.typ_to_string t)))
    | Fix (s, t, e) -> type_expr e (SymTbl.add s t tenv)
    | Seq (e1, e2) ->
        let _ = type_expr e1 tenv in
        type_expr e2 tenv
    | Strct l ->
        let rec stuct_construct = function
          | [] ->
              struct_construct_error
                (List.map (fun (n, e) -> (n, type_expr e tenv)) l)
          | (_, EnumDef _) :: ld -> stuct_construct ld
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
              | Some name -> TStrct name
              | None -> stuct_construct ld)
        in
        stuct_construct prog.types
    | GetF (e, x) -> (
        match type_expr e tenv with
        | TStrct s -> (
            let st = get_struct s in
            try
              let _, t, _ = (List.find (fun (id, _, _) -> id = x)) st in
              t
            with Not_found -> error "The struct a has no field x")
        | t ->
            error
              (Printf.sprintf
                 "This expression has typ %s but was expected a struct\n"
                 (Mmlpp.typ_to_string t)))
    | SetF (e1, x, e2) -> (
        match type_expr e1 tenv with
        | TStrct s -> (
            let st = get_struct s in
            try
              let _, t, m = (List.find (fun (id, _, _) -> id = x)) st in
              check e2 t tenv;
              if m then
                TUnit
              else
                error (Printf.sprintf "%s.%s is not mutable" s x)
            with Not_found -> error "The struct a has no field x")
        | t ->
            error
              (Printf.sprintf
                 "This expression has typ %s but was expected a struct\n"
                 (Mmlpp.typ_to_string t)))
  in

  type_expr prog.code SymTbl.empty
