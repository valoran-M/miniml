open Format
open Mml

let (mem_var: (string, char list) Hashtbl.t) = Hashtbl.create 16
let rec print_list_char = function 
  | [] -> ""
  | c :: l -> Printf.sprintf "%c%s" c (print_list_char l)

let new_type_var =
  let var = ref [char_of_int 96] in
  let rec incr = function
    | [] -> ['a']
    | c::l when c < 'y' -> (char_of_int (int_of_char c + 1)) :: l
    | _::l -> 'a' :: (incr l)
  in
  fun s -> 
    var := incr !var;
    Hashtbl.add  mem_var s !var; 
    !var

let rec typ_to_string = function
  | TInt -> "int"
  | TBool -> "bool"
  | TChar -> "char"
  | TString -> "string"
  | TUnit -> "unit"
  | TRef t -> Printf.sprintf "ref %s" (typ_to_string t)
  | TVar s -> 
    if Hashtbl.mem mem_var s then
      Printf.sprintf "'%s" (print_list_char (Hashtbl.find mem_var s))
    else 
      Printf.sprintf "'%s" (print_list_char (new_type_var s))
  | TFun (typ1, typ2) ->(
      match typ1 with 
      | TFun _ -> 
        Printf.sprintf "(%s) -> %s" 
          (typ_to_string typ1) (typ_to_string typ2)
      | _ -> 
        Printf.sprintf "%s -> %s" 
          (typ_to_string typ1) (typ_to_string typ2))
  | TDef s -> Printf.sprintf "%s" s
  | TParam (t, s) -> Printf.sprintf "(%s %s)" (typ_to_string t) s
  | TArray t -> Printf.sprintf "%s array" (typ_to_string t)

let rec print_fields ppf = function
  | [] -> fprintf ppf ""
  | (x, t, m, _) :: l ->
      let mut =
        if m then
          "mutable "
        else
          ""
      in
      fprintf ppf "%s %s: %s;@ %a" mut x (typ_to_string t) print_fields l

let rec types_list_to_string = function
  | [] -> ""
  | [ t ] -> Printf.sprintf "%s" (typ_to_string t)
  | t :: l -> Printf.sprintf "%s * %s" (typ_to_string t) (types_list_to_string l)

let rec print_constr ppf = function
  | [] -> fprintf ppf ""
  | (s, []) :: l -> 
      fprintf ppf "| %s @. %a" s print_constr l
  | (s, tl) :: l -> 
      fprintf ppf "| %s of (%s) @. %a" s 
        (types_list_to_string (List.map fst tl)) print_constr l

let rec print_types ppf = function
  | [] -> fprintf ppf "@."
  | (t, StrctDef s) :: l ->
      fprintf ppf "type %s = { @[%a}@]@.%a" t print_fields s print_types l
  | (t, AlgDef (e, None)) :: l ->
      fprintf ppf "type %s = @[%a@]@.%a" t print_constr e print_types l
  | (t, AlgDef (e, Some tvar)) :: l ->
      fprintf ppf "type %s %s = @[%a@]@.%a" 
        tvar t print_constr e print_types l

let uop_to_string = function
  | Neg -> "-"
  | Not -> "not "
  | GetRef  -> "!"
  | Alength -> "Array.length "
  | Slength -> "String.length "

let bop_to_string = function
  | Concat  -> "^"
  | Add   -> "+"  | Sub   -> "-"
  | Mul   -> "*"  | Div   -> "/"
  | Mod   -> "mod"
  | Equ   -> "==" | Nequ  -> "!="
  | Sequ  -> "="  | Snequ -> "<>"
  | Lt    -> "<"  | Le    -> "<="
  | Gr    -> ">"  | Gre   -> ">="
  | And   -> "&&" | Or    -> "||"

let typ_print_to_string = function
  | Pt_int      -> "print_int"
  | Pt_bool     -> "print_bool"
  | Pt_newline  -> "print_newline"
  | Pt_char     -> "print_char"
  | Pt_string   -> "print_string"
  | Pt_endline  -> "print_endline"

let rec print_expr ppf = function
  | Int n     -> fprintf ppf "%i" n
  | Bool b    -> fprintf ppf "%b" b
  | Char c    -> fprintf ppf "'%c'" c
  | String s  -> fprintf ppf "%s" s
  | Unit      -> fprintf ppf "()"
  | Ref e     -> fprintf ppf "(ref %a)" print_expr e.expr
  | Var x     -> fprintf ppf "%s" x
  | Array e   -> fprintf ppf "@[<hov2>[|%a|]@]" print_list_expr e
  | Strct l   -> fprintf ppf "@[<hv2>{%a}@]" print_defs l;
  | Constr (s, e) -> fprintf ppf "@[<hov2>%s (%a)@]" s print_list_expr e
  | GetF (e, x)   -> fprintf ppf "(%a).%s" print_expr e.expr x
  | SetRef ((id, _), e) -> 
      fprintf ppf "@[<hov 2>%s :=@; %a@]" id print_expr e.expr
  | Fun (x, Some t, e) ->
      fprintf ppf "@[<hov2>fun (%s: %s) ->@ %a@]" 
        x (typ_to_string t) print_expr e.expr
  | Fun (x, None, e) -> 
      fprintf ppf "@[<hov2>fun (%s) ->@ %a@]" x print_expr e.expr
  | Let (x, e1, _, e2) ->
      fprintf ppf "@[<v>@[<v 2>let %s =@ %a@]@;in@;%a@]"
        x print_expr e1.expr print_expr e2.expr
  | App (e1, e2) -> 
      fprintf ppf "@[<hov2>(%a %a)@]" print_expr e1.expr print_expr e2.expr
  | Uop (op, e) -> 
      fprintf ppf "(%s %a)" (uop_to_string op) print_expr e.expr
  | Bop (op, e1, e2) ->
      fprintf ppf "(@[%a %s %a)@]" print_expr e1.expr 
        (bop_to_string op) print_expr e2.expr
  | If (c, e1, Some e2) ->
      fprintf ppf "@[<hov2>if %a then@ @[%a@]@]@ @[<v 2>else@ %a@]"
        print_expr c.expr print_expr e1.expr print_expr e2.expr
  | If (c, e1, None ) ->
      fprintf ppf "@[<hov2>if %a then@ @[%a@]@]"
        print_expr c.expr print_expr e1.expr
  | Seq (e1, e2) -> 
      fprintf ppf "@[<v>%a;@ %a@]" print_expr e1.expr print_expr e2.expr
    | SetF (e1, x, e2) ->
      fprintf ppf "(%a).%s <- %a" print_expr e1.expr x print_expr e2.expr
  | Fix (x, Some t, e) ->
      fprintf ppf "@[<hov 2>fix (%s: %s) =@ %a@]" 
        x (typ_to_string t) print_expr e.expr
  | Fix (x, None, e) -> fprintf ppf "@[<hov2>fix (%s) =@ %a@]" x print_expr e.expr
  | NArray (e, n) -> 
      fprintf ppf "(@[<hov2>[| %a |]@] * (%a))" print_expr e.expr print_expr n.expr
  | GetI (e, i) -> 
      fprintf ppf "%a.(%a)" print_expr e.expr print_expr i.expr
  | SetI (e1, i, e2) -> 
      fprintf ppf "(%a.(%a) <- (%a))" 
        print_expr e1.expr print_expr i.expr print_expr e2.expr
  | GetS (e, i) -> 
      fprintf ppf "%a.[%a]" print_expr e.expr print_expr i.expr
  | Match (e, l) -> 
      fprintf ppf "@[<v2>match %a with@;@[<v>%a @]" 
        print_expr e.expr print_pat_expr l
  | Print (t, e) -> 
      fprintf ppf "(%s %a)" (typ_print_to_string t) print_expr e.expr

and print_list_expr ppf = function
  | []      -> ()  
  | [ e ]   -> fprintf ppf "%a" print_expr e.expr
  | e :: l  -> fprintf ppf "%a@ , %a" print_expr e.expr print_list_expr l

and print_defs ppf = function
  | []          -> ()
  | (x, e) :: l -> fprintf ppf "@ %s = %a; %a" x print_expr e.expr print_defs l

and print_pat_expr ppf = function
  | [] -> ()
  | [(p, e)] -> 
      fprintf ppf "| @[<hov2>%a ->@ %a@]" 
        print_pattern p.pat print_expr e.expr
  | (p, e) :: l -> 
      fprintf ppf "| @[<hov2>%a ->@ %a@]@ %a" 
        print_pattern p.pat print_expr e.expr print_pat_expr l

and print_pattern ppf = function
  | Pat_jok     -> fprintf ppf "_"
  | Pat_int n   -> fprintf ppf "%d" n
  | Pat_bool b  -> fprintf ppf "%b" b
  | Pat_var v   -> fprintf ppf "%s" v
  | Pat_construct (c, [])   -> fprintf ppf "%s" c
  | Pat_construct (c, l)    -> fprintf ppf "%s(%a)" c print_list_pattern l

and print_list_pattern ppf = function
  | []      -> fprintf ppf ""
  | [ p ]   -> fprintf ppf "%a" print_pattern p.pat
  | p :: l  -> fprintf ppf "%a, %a" print_pattern p.pat print_list_pattern l

let print_prog ppf prog =
  fprintf ppf "%a@.%a@." print_types prog.types print_expr prog.code.expr
