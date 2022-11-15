open Format
open Mml

let bop_to_string = function
  | Add -> "+"
  | Mul -> "*"

let rec print_expr ppf = function
  | Int n -> fprintf ppf "%i" n
  | Bop (op, e1, e2) ->
      fprintf ppf "(@[%a %s %a)@]" print_expr e1 (bop_to_string op) print_expr
        e2

let print_prog ppf prog = fprintf ppf "%a@." print_expr prog.code
