%{
    open Mml
%}

%token <bool> BOOL
%token NOT EQU NEQU LT LE AND OR

%token <int> CST
%token STAR PLUS MINUS DIV MOD

%token SEMI S_PAR E_PAR
%token EOF

%start program
%type <Mml.prog> program

%left SEMI
%left PLUS MINUS
%left MOD
%left STAR DIV

%left OR
%left AND
%left NOT
%left EQU NEQU LT LE

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
