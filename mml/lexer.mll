(* prélude *)
{
    open Parser

    exception Lexing_error of string

    (* hashtable avec tous les mots clefs *)
    let keyword_table = Hashtbl.create 7
    let () =
        List.iter (fun (x, y) -> Hashtbl.add keyword_table x y )         
            [
                "true", BOOL(true);
                "false", BOOL(false);
                "not", NOT;
                "mod", MOD;
                "if", IF;
                "then", THEN;
                "else", ELSE;
                "let", LET;
                "in", IN;
                (* Types *)
                "int", T_INT;
                "bool", T_BOOL;
                "unit", T_UNIT
            ]

    let is_keyword name = Hashtbl.mem keyword_table name
}

let digit = [ '0'-'9' ]
let number = digit+ 
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
let keyword = ['a'-'z']+
let true = "true"
let false = "false"

(* fonction d'analyse *)
rule pattern = parse
    | ['\n']            { pattern lexbuf }
    | [' ' '\t' '\r']+  { pattern lexbuf }
    | "()"              { UNIT_P }
    | "(*"              {comment lexbuf; pattern lexbuf}
    | number as _number {
            CST(int_of_string _number)
        }
    | keyword as name {
            if is_keyword name then
                Hashtbl.find keyword_table name
            else
                IDENT(name)
        }
    | ident as name     { IDENT(name) }
    | "="       { SEQ }
    | "->"      { ARROW }
    | ":"       { COLON }

    (* opérations booléennes *)
    | "=="      { EQU }
    | "!="      { NEQU }
    | "<"       { LT }
    | "<="      { LE }
    | "&&"      { AND }
    | "||"      { OR }

    (* opérations arithmétiques *)
    | "+"       { PLUS }
    | "-"       { MINUS }
    | "*"       { STAR }
    | "/"       { DIV }
    | ";"       { SEMI }
    | "("       { S_PAR }
    | ")"       { E_PAR }
    | eof       { EOF }

and comment = parse
    | "*)"  { () }
    | "(*"  { comment lexbuf; comment lexbuf }
    | _     { comment lexbuf }
    | eof   { raise (Lexing_error "unterminated comment") }
