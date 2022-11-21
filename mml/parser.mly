%{
    open Mml
%}

(* Expressions booléennes *)
%token <bool> BOOL
%token NOT EQU NEQU LT LE AND OR

(* Expressions arithmétiques *)
%token <int> CST
%token STAR PLUS MINUS DIV MOD

(* Condition *)
%token IF THEN ELSE

(* Autres *)
%token SEMI S_PAR E_PAR
%token EOF

%start program
%type <Mml.prog> program


(* Prioritées *)
%nonassoc SEMI
%left PLUS MINUS
%left MOD
%left STAR DIV

%left OR
%left AND
%left NOT
%left EQU NEQU LT LE

%nonassoc THEN
%nonassoc ELSE

%%

program:
    | code=expression EOF       { {code} }
;

simple_expression:
    | n=CST                     { Int n }
    | b=BOOL                    { Bool b }
    | S_PAR e=expression E_PAR  { e } 
;

expression:
    | e=simple_expression                   { e }
    | op=uop e=expression                   { Uop(op, e) }
    | e1=expression op=binop e2=expression  { Bop(op, e1, e2) }
    | IF c=expression THEN e=expression     { If(c, e, Unit) }
    | IF c=expression THEN e1=expression 
                      ELSE e2=expression    { If(c, e1, e2) }
    | e1=expression SEMI e2=expression      { Seq(e1, e2) }
;

%inline uop:
    | MINUS { Neg }
    | NOT   { Not }
;

%inline binop:
    | PLUS  { Add }
    | MINUS { Sub }
    | MOD   { Mod}
    | STAR  { Mul }
    | DIV   { Div }

    | EQU   { Equ }
    | NEQU  { Nequ }
    | LE    { Le }
    | LT    { Lt }
    | OR    { Or }
    | AND   { And }
;
