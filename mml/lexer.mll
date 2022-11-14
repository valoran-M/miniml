(* prélude *)
{
    open Parser
}

let digit = [ '0'-'9' ]
let number = (digit+) 

(* fonction d'analyse *)
rule pattern = parse
    | ['\n'] { pattern lexbuf }
    | [' ' '\t' '\r']+ { pattern lexbuf }
    | number as _number {
        CST(int_of_string _number)
    }
    | "+" { PLUS }
    | "*" { STAR }
    | eof { EOF }