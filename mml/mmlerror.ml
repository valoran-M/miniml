(* Pour remonter des erreurs circonstanciées *)
exception Type_error of string

let error s = raise (Type_error s)

let type_error ty_actual ty_expected =
  error
    (Printf.sprintf
       "expected %s but got %s"
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
    (Printf.sprintf
       "The structure of the form {%s} does not exist"
       (print_struct l))

let struct_no_field s =
  error (Printf.sprintf "The struct a has no field %s\n" s)

let not_a_struct s =
  error
    (Printf.sprintf
       "This expression has typ %s but was expected a struct\n"
       (Mmlpp.typ_to_string s))

let is_not_mutable s x = error (Printf.sprintf "%s.%s is not mutable" s x)

(* Constructeur *)
let unbound_construct name tl =
  error
    (Printf.sprintf
       "Unbound constructor %s(%s)"
       name
       (Mmlpp.types_list_to_string tl))

let nb_arg_construct name n1 n2 =
  error
    (Printf.sprintf
       "The constructor %s expects %d argument(s),\n\
       \ but is applied here to %d argument(s)\n"
       name
       n1
       n2)
