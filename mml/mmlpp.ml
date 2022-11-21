open Format
open Mml

let uop_to_string = function
  | Neg -> "-"
  | Not -> "not "

let bop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "mod"
  | Equ -> "=="
  | Nequ -> "!="
  | Lt -> "<"
  | Le -> "<="
  | And -> "&&"
  | Or -> "||"

let rec print_expr ppf = function
  | Int n -> fprintf ppf "%i" n
  | Bool b -> fprintf ppf "%b" b
  | Unit -> fprintf ppf "()"
  | Uop (op, e) -> fprintf ppf "(@[%s %a@])" (uop_to_string op) print_expr e
  | Bop (op, e1, e2) ->
      fprintf ppf "(@[%a %s %a)@]" print_expr e1 (bop_to_string op) print_expr
        e2
  | If (c, e1, e2) ->
      fprintf ppf "@[if %a then@. @[<hv>%a@]@ else @[<hv>%a@]@]" print_expr c print_expr e1
        print_expr e2
  | Seq (e1, e2) -> fprintf ppf "@[<v>%a;@ %a)@]" print_expr e1 print_expr e2

let print_prog ppf prog = fprintf ppf "%a@." print_expr prog.code
