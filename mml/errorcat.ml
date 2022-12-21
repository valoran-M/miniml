open Format
open Lexing
open Mml

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

let print_line f l fc fl =
  let c = open_in f in
  if(fc <> fl) then (
    for _ = 1 to l - 1 do
      ignore (input_line c)
    done;
    eprintf "%d | %s@." l (input_line c);
    eprintf "%s" (String.make (3 + fc) ' ');
    eprintf "@{<bold>@{<fg_red>%s@}@}@." (String.make (fl - fc) '^')
  )

let report file (l, fc, lc) =
  eprintf "@{<bold>File \"%s\", line %d, characters %d-%d:@}\n" file l fc lc

let print_unclosed_error file l fc lc s =
  add_ansi_marking err_formatter;
  report file (l ,fc ,lc);
  print_line file l fc lc;
  eprintf "@{<bold>@{<fg_red>Error@}@}: lexical error: %s@." s

let print_syntax_err file lb =
  add_ansi_marking err_formatter;
  let pose_s = (lexeme_start_p lb) in
  let fc = pose_s.pos_cnum - pose_s.pos_bol + 1 in
  let pose_e = (lexeme_end_p lb) in
  let lc = pose_e.pos_cnum - pose_e.pos_bol + 1 in
  report file (pose_e.pos_lnum, fc, lc);
  print_line file pose_e.pos_lnum fc lc;
  eprintf "@{<bold>@{<fg_red>Error@}@}: syntax error@."


let print_type_error file e s = 
  add_ansi_marking err_formatter;
  let pose_s = e.loc.fc in
  let fc = pose_s.pos_cnum - pose_s.pos_bol + 1 in
  let pose_e = e.loc.lc in
  let lc = pose_e.pos_cnum - pose_e.pos_bol + 1 in
  report file (pose_e.pos_lnum, fc, lc);
  print_line file pose_e.pos_lnum fc lc;
  eprintf "@{<bold>@{<fg_red>Error@}@}: @[%s@]@." s


