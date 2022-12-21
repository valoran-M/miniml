open Format
open Lexing

(* J'ai suivi cet article:
   https://ocamlpro.com/fr/blog/2020_06_01_fr_tutoriel_format
*)

type style =
    | FG_Red 
    | FG_Default

    | Normal

    | Underline
    | Underline_off

    | Bold
    | Bold_off

let mark_open_stag = function
  | String_tag s -> "<" ^ s ^ ">"
  | _ -> ""

let mark_close_stag = function
  | String_tag s -> "</" ^ s ^ ">"
  | _ -> ""

let print_open_stag = ignore

let print_close_stag = ignore

let close_tag = function
  | Underline -> Underline_off
  | Bold -> Bold_off
  | FG_Red -> FG_Default
  | _ -> Normal

let style_of_tag = function
  | String_tag s -> begin match s with
      | "n"           -> Normal
      | "underline"   -> Underline
      | "/underline"  -> Underline_off
      | "bold"        -> Bold
      | "/bold"       -> Bold

      | "fg_red"      -> FG_Red
      | "fg_default"  -> FG_Default

      | _ -> raise Not_found
    end
  | _ -> raise Not_found

let to_ansi_value = function
  | Normal        -> "0"
  
  | Underline     -> "4"
  | Underline_off -> "24"
  | Bold          -> "1"
  | Bold_off      -> "22"

  | FG_Red        -> "31"
  | FG_Default    -> "39"

let ansi_tag = Printf.sprintf "\x1B[%sm"

let start_mark_ansi_stag t = ansi_tag @@ to_ansi_value @@ style_of_tag t

let stop_mark_ansi_stag t = ansi_tag @@ to_ansi_value @@ close_tag @@ style_of_tag t


let add_ansi_marking formatter =
  let open Format in
  pp_set_mark_tags formatter true;
  let old_fs = pp_get_formatter_stag_functions formatter () in
  pp_set_formatter_stag_functions formatter
    { old_fs with
      mark_open_stag = start_mark_ansi_stag;
      mark_close_stag = stop_mark_ansi_stag 
    }

let report file (b, e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "@{<bold>File \"%s\", line %d, characters %d-%d:@}\n" file l fc lc

let print_lexing_error file lb s =
  report file (lexeme_start_p lb, lexeme_end_p lb);
  eprintf "lexical error: %s@." s

let print_syntax_err file lb =
  add_ansi_marking err_formatter;
  report file (lexeme_start_p lb, lexeme_end_p lb);
  eprintf "@{<bold>@{<fg_red>Error@}@}: syntax error@.";
