%{
    open Mml
%}

%token STAR PLUS
%token <int> CST
%token EOF

%start program
%type <Mml.prog> program

%left PLUS
%left STAR

%%

program:
| code=expression EOF { {code} }
;

expression:
| n=CST { Int(n) }
| e1=expression op=binop e2=expression {Bop(op, e1, e2)}
;

%inline binop:
| PLUS { Add }
| STAR { Mul }
;
