%{
    open Mml

    let rec mk_fun params expr = 
        match params with
        | []            -> expr
        | (id, t) :: l  -> Fun(id, t, mk_fun l expr)
%}

(* Constantes et Varaibles *)
%token <string> IDENT
%token <bool> BOOL
%token <int> CST
%token UNIT_P
%token TYPE 

(* Types *)
%token T_INT T_BOOL T_UNIT MUTABLE

(* Expressions booléennes *)
%token NOT EQU NEQU LT LE AND OR

(* Expressions arithmétiques *)
%token STAR PLUS MINUS DIV MOD

(* Condition *)
%token IF THEN ELSE

(* Autres *)
%token SEMI COLON R_ARROW DOT
%token S_PAR E_PAR S_BRACE E_BRACE
%token LET FUN REC S_EQ IN
%token EOF

%start program
%type <Mml.prog> program


(* Prioritées *)
%nonassoc IN
%right R_ARROW

%left EQU NEQU 
%left LT LE

%left PLUS MINUS
%left MOD
%left STAR DIV

%left OR
%left AND
%nonassoc NOT
%nonassoc S_PAR S_BRACE 
%nonassoc UNIT_P IDENT CST BOOL
%nonassoc THEN
%nonassoc ELSE
%left SEMI

%%

program:
    | l=list(typdes_def) 
        c=expression EOF        { {types = l; code = c} }
;

stuct_types:    
    | m=boption(MUTABLE) id=IDENT COLON t=types SEMI    { (id, t, m) }
;
typdes_def:
    | TYPE id=IDENT S_EQ 
        S_BRACE a=nonempty_list(stuct_types) E_BRACE     { (id, a) }

types:
    | T_INT                     { TInt }
    | T_BOOL                    { TBool }
    | T_UNIT                    { TUnit }
    | id=IDENT                  { TStrct(id) }
    | t1=types R_ARROW t2=types { TFun(t1, t2) }
    | S_PAR t=types E_PAR       { t }
;

stuct_expression:    
    | id=IDENT S_EQ e=expression SEMI   { (id, e) }
;
simple_expression:
    | n=CST                                                 { Int n }
    | b=BOOL                                                { Bool b }
    | UNIT_P                                                { Unit }
    | id=IDENT                                              { Var(id) }
    | S_PAR e=expression E_PAR                              { e }
    | e=simple_expression DOT id=IDENT                      { GetF(e, id) }
    | S_BRACE a=nonempty_list(stuct_expression) E_BRACE     { Strct a }
;

fun_argument:    
    | S_PAR id=IDENT COLON t=types E_PAR    { (id, t) }
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
        R_ARROW e=expression                {Fun(fst a, snd a, e) }
    | LET id=IDENT 
        a=list(fun_argument) S_EQ 
        e1=expression IN 
        e2=expression                       { Let(id, mk_fun a e1, e2) }
    | LET REC id=IDENT a=list(fun_argument) 
        COLON t=types S_EQ 
        e1=expression IN 
        e2=expression                       { Let(id, Fix("f", t, mk_fun a e1), e2) }
    | e1=expression SEMI e2=expression      { Seq(e1, e2) }
;

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
