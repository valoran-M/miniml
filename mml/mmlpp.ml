open Format
open Mml

let rec typ_to_string = function
  | TInt      -> "int"
  | TBool     -> "bool"
  | TUnit     -> "unit"
  | TVar s    -> s
  | TFun (typ1, typ2) ->
      Printf.sprintf "(%s) -> %s" (typ_to_string typ1) (typ_to_string typ2)
  | TStrct s  -> s

let rec print_fields ppf = function
  | [] -> fprintf ppf ""
  | (x, t, m) :: l ->
      let mut =
        if m then
          "mutable "
        else
          ""
      in
      fprintf ppf "%s %s: %s;@ %a" mut x (typ_to_string t) print_fields l

let rec print_enum ppf = function
  | [] -> fprintf ppf ""
  | s::l -> fprintf ppf "| %s @. %a" s print_enum l

let rec print_types ppf = function
  | [] -> fprintf ppf "@."
  | (t, StrctDef s) :: l ->
      fprintf ppf "type %s = { @[%a}@]@.%a" t print_fields s print_types l
  | (t, EnumDef e) :: l ->
      fprintf ppf "type %s = @[%a@]@.%a" t print_enum e print_types l

let uop_to_string = function
  | Neg -> "-"
  | Not -> "not "

let bop_to_string = function
  | Add   -> "+"
  | Sub   -> "-"
  | Mul   -> "*"
  | Div   -> "/"
  | Mod   -> "mod"
  | Equ   -> "=="
  | Nequ  -> "!="
  | Lt    -> "<"
  | Le    -> "<="
  | And   -> "&&"
  | Or    -> "||"

let rec print_expr ppf = function
  | Int n   -> fprintf ppf "%i" n
  | Bool b  -> fprintf ppf "%b" b
  | Unit    -> fprintf ppf "()"
  | Var x   -> fprintf ppf "%s" x
  | Fun (x, t, e) ->
      fprintf ppf "fun (%s: %s) -> %a" x (typ_to_string t) print_expr e
  | Let (x, e1, e2) ->
      fprintf ppf "@[<hv 10>(let %s =@ %a in@.%a)@]" x print_expr e1 print_expr
        e2
  | App (e1, e2)  -> fprintf ppf "(%a %a)" print_expr e1 print_expr e2
  | Uop (op, e)   -> fprintf ppf "(@[%s %a@])" (uop_to_string op) print_expr e
  | Bop (op, e1, e2) ->
      fprintf ppf "(@[%a %s %a)@]" print_expr e1 (bop_to_string op) print_expr
        e2
  | If (c, e1, e2) ->
      fprintf ppf "@[if %a then@. @[<hv 2>%a@]@ else @[<hv 2>%a@]@]" print_expr
        c print_expr e1 print_expr e2
  | Seq (e1, e2)    -> fprintf ppf "@[<v>%a;@ %a@]" print_expr e1 print_expr e2
  | Strct l         -> fprintf ppf "{ @[%a}@]" print_defs l
  | GetF(e, x)      -> fprintf ppf "(%a).%s" print_expr e x
  | SetF(e1, x, e2) -> fprintf ppf "(%a).%s <- %a" print_expr e1 x print_expr e2
  | Fix (x, t, e)   ->
      fprintf ppf "fix (%s: %s) = (%a)" x (typ_to_string t) print_expr e

and print_defs ppf = function
  | [] -> fprintf ppf ""
  | (x, e) :: l -> fprintf ppf "%s = %a; %a" x print_expr e print_defs l

let print_prog ppf prog =
  fprintf ppf "%a@.%a@." print_types prog.types print_expr prog.code
