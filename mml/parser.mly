%{
    open Mml
%}

(* Constantes et Varaibles *)
%token <string> IDENT
%token <bool> BOOL
%token <int> CST
%token UNIT_P

(* Types *)
%token T_INT T_BOOL T_UNIT

(* Expressions booléennes *)
%token NOT EQU NEQU LT LE AND OR

(* Expressions arithmétiques *)
%token STAR PLUS MINUS DIV MOD

(* Condition *)
%token IF THEN ELSE

(* Autres *)
%token SEMI S_PAR E_PAR LET SEQ IN ARROW COLON
%token EOF

%start program
%type <Mml.prog> program


(* Prioritées *)
%nonassoc IN
%nonassoc SEMI
%right ARROW

%left EQU NEQU 
%left LT LE

%left PLUS MINUS
%left MOD
%left STAR DIV

%left OR
%left AND
%nonassoc NOT
%nonassoc S_PAR UNIT_P IDENT CST BOOL
%nonassoc THEN
%nonassoc ELSE

%%

program:
    | code=expression EOF       { {code} }
;

types:
    | T_INT                     { TInt }
    | T_BOOL                    { TBool }
    | T_UNIT                    { TUnit }
    | t1=types ARROW t2=types   { TFun(t1, t2) }
    | S_PAR t=types E_PAR       { t }
;

simple_expression:
    | n=CST                     { Int n }
    | b=BOOL                    { Bool b }
    | UNIT_P                    { Unit }
    | id=IDENT                  { Var(id) }
    | S_PAR e=expression E_PAR  { e }
;

argument:    
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
    | LET id=IDENT a=list(argument) SEQ 
        e1=expression IN 
        e2=expression                       { match a with
                                               | [] ->Let(id, e1, e2) 
                                               | _ -> Let(id, e1, e2)
                                            }
    | e1=expression SEMI e2=expression      { Seq(e1, e2) }
;

%inline uop:
    | MINUS { Neg }
    | NOT   { Not }
;

%inline binop:
    (* Opérations Arithmétiques *)
    | PLUS  { Add }
    | MINUS { Sub }
    | MOD   { Mod}
    | STAR  { Mul }
    | DIV   { Div }
    (* Opérations Booléennes *)
    | EQU   { Equ }
    | NEQU  { Nequ }
    | LE    { Le }
    | LT    { Lt }
    | OR    { Or }
    | AND   { And }
;
