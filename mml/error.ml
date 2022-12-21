(* Pour remonter des erreurs circonstanciées *)
type error =
    | Lexing_error of string
    | Type_error of string

exception Error of error

let raise_type_error s = raise (Error (Type_error s))

let type_error ty_actual ty_expected =
  raise_type_error
    (Printf.sprintf
       "expected %s but got %s"
       (Mmlpp.typ_to_string ty_expected)
       (Mmlpp.typ_to_string ty_actual))

(* Fonction *)
let not_a_function t =
  raise_type_error  
    (Printf.sprintf
      "This expression has typ %s\n\
        This is not a function; it cannot be applied.\n"
      (Mmlpp.typ_to_string t))


(* Structure *)
let struct_construct_error l =
  let rec print_struct = function
    | [] -> ""
    | (n, t) :: l ->
        Format.sprintf "%s: %s ; %s" 
          n (Mmlpp.typ_to_string t) (print_struct l)
  in
  raise_type_error
    (Printf.sprintf
       "The structure of the form {%s} does not exist"
       (print_struct l))

let struct_no_field s =
  raise_type_error (Printf.sprintf "The struct a has no field %s\n" s)

let not_a_struct s =
  raise_type_error
    (Printf.sprintf
       "This expression has typ %s but was expected a struct\n"
       (Mmlpp.typ_to_string s))

let is_not_mutable s x =
  raise_type_error (Printf.sprintf "%s.%s is not mutable" s x)

(* Constructeur *)
let unbound_construct name tl =
  raise_type_error
    (Printf.sprintf
       "Unbound constructor %s(%s)"
       name
       (Mmlpp.types_list_to_string tl))

let nb_arg_construct name n1 n2 =
  raise_type_error
    (Printf.sprintf
       "The constructor %s expects %d argument(s),\n\
       \ but is applied here to %d argument(s)\n"
       name n1 n2)

let compare_fun () = raise_type_error "compare: functional value"
