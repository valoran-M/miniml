%{
    open Lexing
    open Mml

    let mk_loc (fc, lc) = 
      {fc = fc; lc = lc}

    let mk_expr loc expr =
      {loc = mk_loc loc; expr}

    let mk_pat loc pat =
      {loc = mk_loc loc; pat}

    let rec mk_fun params expr = 
        match params with
        | []            -> expr
        | (loc, id, t) :: l  -> mk_expr loc (Fun(id, t, mk_fun l expr))

    let unclosed_error (fc, lc) s sexp = 
      let s = s ^ " unclosed : " ^ sexp ^ " expected" in 
      Error.raise_unclosed [(fc.pos_lnum, fc.pos_cnum - fc.pos_bol, 
                                            lc.pos_cnum - lc.pos_bol)] s

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
          | (_, _, Some t')::xs -> TFun(t', aux xs t)
          | (_, _, None)::xs -> TFun (TVar (new_var ()), aux xs t)
        in 
        Some (aux xs t)
%}

(* Constantes et Varaibles *)
%token <string> IDENT       "x"
%token <bool> BOOL          "b"
%token <int> CST            "n"
%token <char> CHAR          "'c'"
%token <string> STRING      "\"exemple\""
%token UNIT                 "()"
(* Types *)
%token <string>T_VAR        "'a"
%token T_INT                "int"
%token T_BOOL               "bool"
%token T_UNIT               "unit"
%token MUTABLE              "mutable"
%token <string> CONSTR      "Uid"
(* match *)
%token MATCH    "match"
%token WITH     "with"
(* Expressions booléennes *)
%token NOT      "!"
%token EQU      "=="
%token NEQU     "!="
%token LT       "<"
%token LE       "<="
%token GR       ">"
%token GRE      ">="
%token AND      "&&"
%token OR       "||"
%token S_EQ     "="
%token DIFF     "<>"
(* Expressions arithmétiques *)
%token STAR     "*"
%token PLUS     "+"
%token MINUS    "-"
%token DIV      "/"
%token MOD      "mod"
(* Condition *)
%token IF       "if"
%token THEN     "then"
%token  ELSE    "else"
(* Autres *)
%token SEMI           ";"
%token COLON          ":"
%token R_ARROW        "<-"
%token L_ARROW        "->"
%token DOT            "."
%token S_PAR          "("
%token E_PAR          ")"
%token S_BRACE        "{"
%token E_BRACE        "}"
%token S_BRACKETBAR   "[|"
%token E_BRACKETBAR   "|]"
%token A_CREATE       "Array.create"
%token LET            "let"
%token FUN            "fun"
%token REC            "rec"
%token IN             "in"
%token BAR            "|"
%token OF             "of"
%token COMMA          ","
%token EOF            ""
%token TYPE           "type"
%token PRINT_INT      "print_int"
%token PRINT_BOOL     "print_bool"
%token PRINT_CHAR     "print_char"
%token PRINT_STRING   "print_string"
%token PRINT_ENDLINE  "print_endline"
%token PRINT_NEWLINE  "print_newline"

%start program
%type <Mml.prog> program
%type <Mml.expr_loc> expression


(* Prioritées *)
%nonassoc less_prio
%nonassoc SEMI
%nonassoc T_VAR
%nonassoc L_ARROW
%right    R_ARROW               (* type(t -> t -> t) *)
%nonassoc THEN                  (* BELLOW else if ... then ... *)
%nonassoc ELSE                  (* if ... then ... else ... *)
%left     BAR
%left     OR                    (* expr( e || e || e) *)
%left     AND                   (* expr( e && e && e) *)
%nonassoc NOT                   (* expr *)

%left     EQU NEQU DIFF S_EQ    (* expr( e == e == e) *)
%left     LT LE GR GRE          (* expr( e < e < e) *)

%left     PLUS MINUS            (* expr( e + e + e) *)
%left     MOD                   (* expr( e mod e mod e) *)
%left     STAR DIV              (* expr( e * e * e) *)

%nonassoc prec_constr_empty     (* C vs C (x) *)
(* Autres *)
%nonassoc S_PAR
          IDENT

%%

program:
    | l=list(types_def) 
        c=expr_seq EOF        
      { {types = l; code = c} }
;

simple_expression:
    | n=CST       { mk_expr $sloc (Int n) }
    | b=BOOL      { mk_expr $sloc (Bool b) }
    | c=CHAR      { mk_expr $sloc (Char c)}
    | s=STRING    { mk_expr $sloc (String(s)) }
    | UNIT        { mk_expr $sloc Unit }
    | id=IDENT    { mk_expr $sloc (Var (id)) }
    | S_PAR e=expr_seq E_PAR                        
      { e }
    | S_PAR expr_seq error 
      { unclosed_error ($loc($1)) "\"(\"" "\")\"" }
    | e=simple_expression DOT id=IDENT              
      { mk_expr $sloc (GetF (e, id)) }
    | e=simple_expression DOT S_PAR i=expr_seq E_PAR 
      { mk_expr $sloc (GetI(e, i)) }
    | S_BRACE a=nonempty_list(body_struct) E_BRACE  
      { mk_expr $sloc (Strct a) }
    | S_BRACE nonempty_list(body_struct) error
      { unclosed_error ($loc($1)) "\"{\"" "\"}\"" }
    | S_BRACKETBAR l=separated_list(SEMI, expression) E_BRACKETBAR
      { mk_expr $sloc (Array l) }
    | S_BRACKETBAR separated_list(SEMI, expression) error
      { unclosed_error ($loc($1)) "\"[|\"" "\" |]\""  }
    | id=CONSTR l=constr_param  
      { mk_expr $sloc (Constr (id, l)) }
    
;

expr_seq:
    | e=expression %prec less_prio    { e }
    | e1=expression SEMI e2=expr_seq  { mk_expr $sloc (Seq(e1, e2)) }
;

app_expr:
    | e=simple_expression { e }
    | e=app_expr a=simple_expression
        { mk_expr $sloc (App(e, a)) }

expression:
    | e=simple_expression                       
        { e }
    | op=uop e=expression                       
        { mk_expr $sloc (Uop(op, e)) }
    | e1=expression op=binop e2=expression      
        { mk_expr $sloc (Bop(op, e1, e2)) }
    | e=app_expr l=simple_expression
        { mk_expr $sloc (App(e, l)) }
    | IF c=expr_seq THEN e=expression           
        { mk_expr $sloc (If(c, e, None)) }
    | IF c=expr_seq THEN e1=expression 
                    ELSE e2=expression
        { mk_expr $sloc (If(c, e1, Some e2)) }
    | FUN a=fun_argument R_ARROW e=expr_seq                
        { 
          let _, id, t = a in 
          mk_expr $sloc (Fun(id, t, e)) 
        }
    | e1=simple_expression DOT id=IDENT L_ARROW e2=expression 
        { mk_expr $sloc (SetF(e1, id, e2)) }
    | e1=simple_expression DOT S_PAR i=expr_seq E_PAR L_ARROW e2=expression
        { mk_expr $sloc (SetI(e1, i, e2))}
    | A_CREATE n=simple_expression e=simple_expression
        { mk_expr $sloc (NArray(e, n)) }
    | e=let_expr 
        { mk_expr $sloc e }
    | MATCH e=expression WITH l=nonempty_list(pattern_expr)
        { mk_expr $sloc (Match (e, l)) }
    | p=print_type e=simple_expression 
        { mk_expr $sloc (Print(p, e)) }
;

(* types *)
types:
    | T_INT                     { TInt }
    | T_BOOL                    { TBool }
    | T_UNIT                    { TUnit }
    | t=T_VAR                   { TVar(t) }
    | t=T_VAR id=IDENT          { TParam(TVar(t), id) }
    | id=IDENT                  { TDef(id) }
    | t1=types R_ARROW t2=types { TFun(t1, t2) }
    | S_PAR t=types E_PAR       { t }
;
types_def:
    | s=struct_def    { s }
    | c=constr_def    { c }
;

(* Déclaration/fonction *)
%inline let_expr:
    | LET r=boption(REC) id=IDENT 
          a=list(let_argument) 
          t=option(type_forcing) S_EQ 
          e1=expr_seq IN 
          e2=expr_seq                   
        { 
          if r then (
            let t = mk_fun_type a t in
            Let(id, mk_expr $sloc (Fix(id, 
              t, mk_fun a e1)), t, e2) 
          )else 
            Let(id, mk_fun a e1, mk_fun_type a t, e2) 
        }

let_argument:    
    | S_PAR id=IDENT t=type_forcing E_PAR   { ($sloc, id, Some t) }
    | id=IDENT                              { ($sloc, id, None) }
    | S_PAR E_PAR                           { ($sloc, "", Some TUnit) }
;
type_forcing:
    COLON t=types {t}
;
%inline fun_argument:
  a=let_argument  { a }

(* Structure *)
%inline struct_def:
    | TYPE id=IDENT S_EQ 
        S_BRACE a=nonempty_list(body_struct_def) E_BRACE                         
      { (id, StrctDef a) }
;
body_struct_def:
    | m=boption(MUTABLE) id=IDENT COLON t=types SEMI
      { (id, t, m, mk_loc $sloc) }
    | boption(MUTABLE) IDENT COLON types error
      { Error.raise_missing_semi (mk_loc ($loc($5))) }
;

body_struct:    
    | id=IDENT S_EQ e=expression SEMI
      { (id, e) }
    | IDENT S_EQ expression error
      { Error.raise_missing_semi (mk_loc ($loc($4))) }
;

(* Constructeur *)
%inline constr_types_def:
    | t=types           
        {t, mk_loc $sloc}
;
constr_types: 
    | BAR c=CONSTR                                
      { (c, []) }
    | BAR c=CONSTR OF 
        l=separated_nonempty_list(STAR, constr_types_def)    
      { (c, l) }
;

%inline constr_def:
    | TYPE id=IDENT S_EQ 
        a=nonempty_list(constr_types)             
      { (id, AlgDef (a, None)) }
    | TYPE t=T_VAR id=IDENT S_EQ 
        a=nonempty_list(constr_types)             
      { (id, AlgDef (a, Some t)) }
;

constr_param:
    | (* empty *)   %prec prec_constr_empty
      { [] }
    | S_PAR l=separated_nonempty_list(COMMA, expression) E_PAR
      { l }
;

(* pattern matching *)
pattern:
    | n=CST 
      { mk_pat $sloc (Pat_int n) }
    | id=IDENT 
      { mk_pat $sloc (if id = "_" then Pat_jok else Pat_var id)}
    | b=BOOL
      { mk_pat $sloc (Pat_bool b) }
    | c=CONSTR
      { mk_pat $sloc (Pat_construct(c, [])) }
    | c=CONSTR S_PAR l=separated_nonempty_list(COMMA, pattern) E_PAR
      { mk_pat $sloc (Pat_construct(c, l)) }
;
%inline pattern_expr:
    | BAR p=pattern R_ARROW e=expr_seq
      { (p, e) }
;

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
    | S_EQ  { Sequ }| DIFF  { Snequ }
    | LE    { Le }  | LT    { Lt }
    | GRE   { Gre}  | GR    { Gr }
    | OR    { Or }  | AND   { And }
;

(* print *)
%inline print_type:
  | PRINT_INT     { Pt_int }
  | PRINT_BOOL    { Pt_bool}
  | PRINT_CHAR    { Pt_char }
  | PRINT_STRING  { Pt_string }
  | PRINT_ENDLINE { Pt_endline }
  | PRINT_NEWLINE { Pt_newline }
