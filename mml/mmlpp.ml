open Format
open Mml

let uop_to_string = function
  | Neg -> "-"  | Not -> "not "

let bop_to_string = function
  | Add -> "+"  | Sub -> "-"
  | Mul -> "*"  | Div -> "/"
  | Mod -> "mod"
  | Equ -> "==" | Nequ -> "!="
  | Lt -> "<"   | Le -> "<="
  | And -> "&&" | Or -> "||"

let rec typ_to_string = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TFun (typ1, typ2) ->
      Printf.sprintf "(%s) -> %s" (typ_to_string typ1) (typ_to_string typ2)

let rec print_expr ppf = function
  | Int n -> fprintf ppf "%i" n
  | Bool b -> fprintf ppf "%b" b
  | Unit -> fprintf ppf "()"
  | Var x -> fprintf ppf "%s" x
  | Fun (x, t, e) ->
      fprintf ppf "fun (%s: %s) -> %a" x (typ_to_string t) print_expr e
  | Let (x, e1, e2) ->
      fprintf ppf "@[<hv 10>(let %s =@ %a in@.%a)@]" x print_expr e1 print_expr
        e2
  | App (e1, e2) -> fprintf ppf "(%a %a)" print_expr e1 print_expr e2
  | Uop (op, e) -> fprintf ppf "(@[%s %a@])" (uop_to_string op) print_expr e
  | Bop (op, e1, e2) ->
      fprintf ppf "(@[%a %s %a)@]" print_expr e1 (bop_to_string op) print_expr
        e2
  | If (c, e1, e2) ->
      fprintf ppf "@[if %a then@. @[<hv 2>%a@]@ else @[<hv 2>%a@]@]" print_expr
        c print_expr e1 print_expr e2
  | Seq (e1, e2) -> fprintf ppf "@[<v>%a;@ %a@]" print_expr e1 print_expr e2
  | Fix(x, t, e) -> fprintf ppf "fix (%s: %s) = (%a)" x (typ_to_string t) print_expr e

let print_prog ppf prog = fprintf ppf "%a@." print_expr prog.code
