(* Pour remonter des erreurs circonstanciées *)
type error =
    | Unclosed      of int * int * int * string
    | Missing_semi  of Mml.location
    | Type_error    of Mml.location * string
    | Type_def      of Mml.location * string
    | Match_failure of Mml.location * (int, Value.heap_value) Hashtbl.t * Value.value
    | Invalid_argument of string

exception Error of error

let raise_type_error e s = raise (Error (Type_error (e, s)))

let type_error e ty_actual ty_expected =
  raise_type_error e
    (Printf.sprintf
       "This expression has type %s but an expression was expected of type\n\
         \t%s"
       (Mmlpp.typ_to_string ty_actual)
       (Mmlpp.typ_to_string ty_expected))

let unbound_record_field e s = 
    raise_type_error e 
      (Printf.sprintf "Unbound record field %s" s)

let unbound_value e s lv =
  raise_type_error e 
    (Printf.sprintf "Unbound value %s\n%s" s (Utils.did_you_mean s lv))

(* match *)
let raise_match_failure loc mem v = 
  raise (Error (Match_failure (loc, mem, v)))

(* semi *)
let raise_missing_semi loc = 
  raise (Error(Missing_semi loc))

(* unclosed *)
let raise_unclosed l s = 
  match l with 
  | [] -> assert false
  | (l, fc, lc) :: _ -> raise (Error (Unclosed (l, fc, lc, s)))

(* Fonction *)
let not_a_function e t =
  raise_type_error e
    (Printf.sprintf
      "This expression has typ %s\n\
        This is not a function; it cannot be applied.\n"
      (Mmlpp.typ_to_string t))


(* Structure *)
let struct_construct_error e l =
  let rec print_struct = function
    | [] -> ""
    | (n, t) :: l ->
        Format.sprintf "%s: %s ; %s" 
          n (Mmlpp.typ_to_string t) (print_struct l)
  in
  raise_type_error e
    (Printf.sprintf
       "The structure of the form {%s} does not exist"
       (print_struct l))

let struct_no_field e t s =
  raise_type_error e 
    (Printf.sprintf 
      "This expression hase type %s There is no field %s within type %s\n" 
      t s t)

let not_a_struct e s =
  raise_type_error e
    (Printf.sprintf
       "This expression has typ %s but was expected a struct\n"
       (Mmlpp.typ_to_string s))

let is_not_mutable e s x =
  raise_type_error e (Printf.sprintf "%s.%s is not mutable" s x)

(* Constructeur *)
let unbound_construct e name tl =
  raise_type_error e
    (Printf.sprintf
       "Unbound constructor %s(%s)"
       name
       (Mmlpp.types_list_to_string tl))

let nb_arg_construct e name n1 n2 =
  raise_type_error e
    (Printf.sprintf
       "The constructor '%s' expects %d argument(s),\n\
       \ but is applied here to %d argument(s)\n"
       name n1 n2)

let compare_fun e  = 
  raise_type_error e "compare functional value"

(* types def *)

let struct_undefined_type s t = 
  Printf.sprintf 
      "The definition of struct '%s' contains this undefined %s type" 
      s t

let struct_same_label_name s = 
  Printf.sprintf "Two labels are names %s" s

let struct_def_error loc s = 
  raise (Error (Type_def (loc, s)))
    

let constr_def_error name loc t = 
  raise (Error (Type_def (loc,
    (Printf.sprintf 
      "The definition of '%s' contains this undefined %s type" 
      name t))))
