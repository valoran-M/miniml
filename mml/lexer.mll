(* prélude *)
{
    open Parser

    exception Lexing_error of string
}

let digit = [ '0'-'9' ]
let number = digit+ 
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
let true = "true"
let false = "false"

(* fonction d'analyse *)
rule pattern = parse
    | ['\n']            { pattern lexbuf }
    | [' ' '\t' '\r']+  { pattern lexbuf }
    | "(*"              {comment lexbuf; pattern lexbuf}
    | number as _number {
            CST(int_of_string _number)
        }
    | "true"    { BOOL(true) }
    | "false"   { BOOL(false) }

    (* opérations booléennes *)
    | "not" { NOT }
    | "=" { EQU }
    | "!=" { NEQU }
    | "<" { LT }
    | "<=" { LE }
    | "&&" { AND }
    | "||" { OR }

    (* opérations arithmétiques *)
    | "+"       { PLUS }
    | "-"       { MINUS }
    | "mod"     { MOD }
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
