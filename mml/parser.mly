%{
    open Mml
%}

%token STAR PLUS MINUS DIV 
%token SEMI S_PAR E_PAR
%token <bool> BOOL
%token <int> CST
%token EOF

%start program
%type <Mml.prog> program

%left SEMI
%left PLUS MINUS
%left STAR DIV

%%

program:
| code=expression EOF { {code} }
;

simple_expression:
| n=CST     { Int(n) }
| b=BOOL    { Bool(b) }

expression:
| e=simple_expression { e }
| op=uop e=expression { Uop(op, e) }
| e1=expression op=binop e2=expression { Bop(op, e1, e2) }
| e1=expression SEMI e2=expression { Seq(e1, e2) }
;

%inline uop:
| MINUS { Neg }

%inline binop:
| PLUS  { Add }
| MINUS { Sub }
| STAR  { Mul }
| DIV   { Div }
;
