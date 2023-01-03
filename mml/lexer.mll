(* prélude *)
{
    open Lexing
    open Parser

    (* hashtable avec tous les mots clefs *)
    let keyword_table = Hashtbl.create 22
    let () =
        List.iter (fun (x, y) -> Hashtbl.add keyword_table x y )         
            [
                "true", BOOL(true);
                "false", BOOL(false);
                (* calcul *)
                "not", NOT;
                "mod", MOD;
                (* conditions *)
                "if", IF;
                "then", THEN;
                "else", ELSE;
                (* match *)
                "match", MATCH;
                "with", WITH;
                (* function *)
                "let", LET;
                "fun", FUN;
                "rec", REC;
                "in", IN;
                (* Types *)
                "int", T_INT;
                "bool", T_BOOL;
                "unit", T_UNIT;
                "type", TYPE;
                "mutable", MUTABLE;
                "of", OF;
                
            ]

    let is_keyword name = Hashtbl.mem keyword_table name

    let id_to_lexem = function
      | "print_int" -> PRINT_INT
      | "print_bool" -> PRINT_BOOL
      | "print_newline" -> PRINT_NEWLINE
      | "print_char" -> PRINT_CHAR
      | "print_string" -> PRINT_STRING
      | "print_endline" -> PRINT_ENDLINE
      | s -> IDENT(s)

    let string_buffer = Buffer.create 256
    let reset_stored_string () = Buffer.reset string_buffer
    let get_stored_string () = Buffer.contents string_buffer
    let store_string_char c = Buffer.add_char string_buffer c

    let l = ref []

    let begin_comment lexbuf =
      let start = lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol in
      l := (lexbuf.lex_start_p.pos_lnum, start, start + 2) :: !l
    
    let begin_string lexbuf = 
      let start = lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol in
      l := (lexbuf.lex_start_p.pos_lnum, start, start + 1) :: !l

    let end_bloc () =
      match !l with 
      | [] -> assert false
      | _::ls -> l := ls 
}

let digit = [ '0'-'9' ]
let number = digit+ 
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
let construct = ['A'-'Z'] (alpha | '_' | digit)*
let keyword = ['a'-'z']+
let tvar = '\''['a'-'z']+
let true = "true"
let false = "false"

(* fonction d'analyse *)
rule pattern = parse
    | ['\n']            { new_line lexbuf; pattern lexbuf }
    | [' ' '\t' '\r']+  { pattern lexbuf }
    | "(*"              
      { begin_comment lexbuf; comment lexbuf; pattern lexbuf}
    | number as _number {
            CST(int_of_string _number)
        }
    | construct as _construct {
            CONSTR(_construct)
        }
    | keyword as name {
            if is_keyword name then
                Hashtbl.find keyword_table name
            else
                IDENT(name)
        }
    | ident as name     { id_to_lexem name }
    | tvar  as var      { T_VAR(var) } 
    | "Array.make"      { A_CREATE }
    | '\"'
      { 
        begin_string lexbuf;
        string lexbuf;
        let s  = get_stored_string () in
        reset_stored_string ();
        STRING(s)
      }
    (* char *)
    | "'\n'"
      { new_line lexbuf; CHAR('\n')}
    | "'\\" (['\\' '\'' '\"'] as c) "'"
      { CHAR(c) }
    | "'\\n'" { CHAR('\n') }
    | "'\\t'" { CHAR('\t') }
    | "'\\r'" { CHAR('\r') }
    | "'" (_ as c) "'"  { CHAR(c) }
    (* symboles *)
    | "="       { S_EQ }
    | "->"      { R_ARROW }
    | "<-"      { L_ARROW }
    | ":"       { COLON }
    | ";"       { SEMI }
    | ","       { COMMA }
    | "."       { DOT }
    | "|"       { BAR }
    (* opérations booléennes *)
    | "=="      { EQU }
    | "!="      { NEQU }
    | "<>"      { DIFF }
    | "<"       { LT }
    | "<="      { LE }
    | ">"       { GR }
    | ">="      { GRE }
    | "&&"      { AND }
    | "||"      { OR }
    (* opérations arithmétiques *)
    | "+"       { PLUS }
    | "-"       { MINUS }
    | "*"       { STAR }
    | "/"       { DIV }
    (* PAR et BRACE *)
    | "()"      { UNIT }
    | "("       { S_PAR }
    | ")"       { E_PAR }
    | "{"       { S_BRACE }
    | "}"       { E_BRACE }
    | "[|"      { S_BRACKETBAR }
    | "|]"      { E_BRACKETBAR }
    | "["       { S_BRACKET }
    | "]"       { E_BRACKET }
    | eof       { EOF }

and comment = parse
    | "*)"  { end_bloc () }
    | "(*"  { begin_comment lexbuf; comment lexbuf; comment lexbuf }
    | ['\n']{ new_line lexbuf; comment lexbuf }
    | _     { comment lexbuf }
    | eof   { Error.raise_unclosed (!l) "unterminated comment" }

and string = parse 
    | '\\' (['\\' '\'' '\"'] as c )
        { store_string_char c; string lexbuf }
    | "\\n"
        { store_string_char '\n'; string lexbuf }
    | "\\t"
        { store_string_char '\t'; string lexbuf }
    | "\\r"
        { store_string_char '\r'; string lexbuf }
    | '\"'
        { end_bloc () }
    | "\n"
        { Error.raise_unclosed (!l) "unterminated string" }
    | eof 
        { Error.raise_unclosed (!l) "unterminated string" }
    | _ as c 
        { store_string_char c; string lexbuf}
