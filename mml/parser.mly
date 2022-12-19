%{
    open Mml

    let rec mk_fun params expr = 
        match params with
        | []            -> expr
        | (id, t) :: l  -> Fun(id, t, mk_fun l expr)

    let mk_fun_type xs t = 
      match t with
      | None -> None
      | Some t -> 
        let rec aux xs t =
        let new_var =
          let cpt = ref 0 in
          fun () ->
            incr cpt;
            Printf.sprintf "tvar_%i" !cpt
          in
          match xs with
          | [] -> t
          | (_, Some t')::xs -> TFun(t', aux xs t)
          | (_, None)::xs -> TFun (TVar (new_var ()), aux xs t)
        in 
        Some (aux xs t)
%}

(* Constantes et Varaibles *)
%token <string> IDENT
%token <bool> BOOL
%token <int> CST
%token UNIT_P
%token TYPE 
%token <string> CONSTR

(* Types *)
%token T_INT T_BOOL T_UNIT MUTABLE

(* Expressions booléennes *)
%token NOT EQU NEQU LT LE AND OR

(* Expressions arithmétiques *)
%token STAR PLUS MINUS DIV MOD

(* Condition *)
%token IF THEN ELSE

(* Autres *)
%token SEMI COLON R_ARROW L_ARROW DOT
%token S_PAR E_PAR S_BRACE E_BRACE
%token LET FUN REC S_EQ IN
%token BAR OF COMMA
%token EOF 

%start program
%type <Mml.prog> program


(* Prioritées *)
%nonassoc   IN
%nonassoc   UNIT_P IDENT CST BOOL
%left       SEMI
%nonassoc   L_ARROW
%nonassoc   R_ARROW 
%nonassoc   THEN
%nonassoc   ELSE

%left       EQU NEQU 
%left       LT LE

%left       PLUS MINUS
%left       MOD
%left       STAR DIV

%left       OR
%left       AND
%nonassoc   NOT

%nonassoc   prec_constr_empty
%nonassoc   S_PAR S_BRACE CONSTR

%%

program:
    | l=list(typdes_def) 
        c=expression EOF        { {types = l; code = c} }
;

simple_expression:
    | n=CST                                                 { Int n }
    | b=BOOL                                                { Bool b }
    | UNIT_P                                                { Unit }
    | id=IDENT                                              { Var (id) }
    | S_PAR e=expression E_PAR                              { e }
    | e=simple_expression DOT id=IDENT                      { GetF (e, id) }
    | S_BRACE a=nonempty_list(body_struct) E_BRACE          { Strct a }
    | id=CONSTR l=constr_param                              { Constr (id, l) }
;

expression:
    | e=simple_expression                   { e }
    | op=uop e=expression                   { Uop(op, e) }
    | e1=expression op=binop e2=expression  { Bop(op, e1, e2) }
    | e=expression se=simple_expression     { App(e, se) }
    | IF c=expression THEN e=expression     { If(c, e, Unit) }
    | IF c=expression THEN e1=expression 
                      ELSE e2=expression    { If(c, e1, e2) }
    | FUN a=fun_argument 
        R_ARROW e=expression                { Fun(fst a, snd a, e) }
    | e1=simple_expression DOT id=IDENT 
        L_ARROW e2=expression               { SetF(e1, id, e2) }
    | e1=expression SEMI e2=expression      { Seq(e1, e2) }
    | e=let_expr                            { e }
    
;

(* types *)
types:
    | T_INT                     { TInt }
    | T_BOOL                    { TBool }
    | T_UNIT                    { TUnit }
    | id=IDENT                  { TStrct(id) }
    | t1=types R_ARROW t2=types { TFun(t1, t2) }
    | S_PAR t=types E_PAR       { t }
;
typdes_def:
    | s=struct_def    { s }
    | c=constr_def    { c }
;

(* Fonction *)
%inline let_expr:
    | LET id=IDENT 
        a=list(let_argument) S_EQ 
        e1=expression IN 
        e2=expression                       { Let(id, mk_fun a e1, e2) }
    | LET REC id=IDENT a=list(let_argument) 
        t=option(type_forcing) S_EQ 
        e1=expression IN 
        e2=expression                       { 
                                                Let(id, Fix(id, 
                                                            mk_fun_type a t,
                                                            mk_fun a e1), e2) 
                                            }
let_argument:    
    | S_PAR id=IDENT t=type_forcing E_PAR   { (id, Some t) }
    | id=IDENT                              { (id, None) }
;
type_forcing:
  COLON t=types {t}
;

(* Structure *)
%inline struct_def:
    | TYPE id=IDENT S_EQ 
        S_BRACE 
          a=nonempty_list(body_struct_def) 
        E_BRACE                         { (id, StrctDef a) }

body_struct_def:
    | m=boption(MUTABLE) id=IDENT COLON t=types SEMI    { (id, t, m) }
;

body_struct:    
    | id=IDENT S_EQ e=expression SEMI   { (id, e) }
;

(* Constructeur *)
constr_types: 
    | BAR c=CONSTR                                { (c, []) }
    | BAR c=CONSTR OF 
        l=separated_nonempty_list(STAR, types)    { (c, l) }
;

%inline constr_def:
    | TYPE id=IDENT S_EQ 
        a=nonempty_list(constr_types)             { (id, ConstrDef a) }
;

constr_param:
    | (* empty *)   %prec prec_constr_empty
        { [] }
    | S_PAR l=separated_nonempty_list(COMMA, expression) E_PAR
        { l }

(* Opération *)
%inline uop:
    | MINUS { Neg }
    | NOT   { Not }
;

%inline binop:
    (* Opérations Arithmétiques *)
    | PLUS  { Add } | MINUS { Sub }
    | MOD   { Mod}
    | STAR  { Mul } | DIV   { Div }
    (* Opérations Booléennes *)
    | EQU   { Equ } | NEQU  { Nequ }
    | LE    { Le }  | LT    { Lt }
    | OR    { Or }  | AND   { And }
;

%inline fun_argument:
  a=let_argument  { a }
