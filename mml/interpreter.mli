type value =
    | VInt of int
    | VBool of bool
    | VUnit
    | VPtr of int

val print_value: value -> unit

val eval_prog : Mml.prog -> value

