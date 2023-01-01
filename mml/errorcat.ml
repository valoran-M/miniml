open Format
open Lexing
open Mml

(* J'ai suivi cet article:
   https://ocamlpro.com/fr/blog/2020_06_01_fr_tutoriel_format *)

type style =
    | FG_Red
    | FG_Mag
    | FG_Default
    | Normal
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
  | Bold -> Bold_off
  | FG_Red -> FG_Default
  | FG_Mag -> FG_Default
  | _ -> Normal

let style_of_tag = function
  | String_tag s -> (
      match s with
      | "n" -> Normal
      | "bold" -> Bold
      | "/bold" -> Bold
      | "fg_red" -> FG_Red
      | "fg_mag" -> FG_Mag
      | "fg_default" -> FG_Default
      | _ -> raise Not_found)
  | _ -> raise Not_found

let to_ansi_value = function
  | Normal -> "0"
  | Bold -> "1"
  | Bold_off -> "22"
  | FG_Red -> "31"
  | FG_Mag -> "35"
  | FG_Default -> "39"

let ansi_tag = Printf.sprintf "\x1B[%sm"

let start_mark_ansi_stag t = ansi_tag @@ to_ansi_value @@ style_of_tag t

let stop_mark_ansi_stag t =
  ansi_tag @@ to_ansi_value @@ close_tag @@ style_of_tag t

let add_ansi_marking formatter =
  let open Format in
  pp_set_mark_tags formatter true;
  let old_fs = pp_get_formatter_stag_functions formatter () in
  pp_set_formatter_stag_functions
    formatter
    { old_fs with
      mark_open_stag = start_mark_ansi_stag
    ; mark_close_stag = stop_mark_ansi_stag
    }

let print_prog_lines c fl ll fc lc =
  let make_alig i =
    String.make
      (int_of_float (log10 (float_of_int ll))
      - int_of_float (log10 (float_of_int i)))
      ' '
  in
  let line = input_line c in
  let start = String.make fc '.' in
  eprintf
    "%s%d | %s%s@."
    (make_alig fl) fl start
    (String.sub line fc (String.length line - fc));
  for i = fl + 1 to ll - 1 do
    eprintf "%s%d | %s@." (make_alig i) i (input_line c)
  done;
  let line = input_line c in
  let start = String.sub line 0 lc in
  eprintf "%d | %s%s@." ll start (String.make (String.length line - lc) '.')

let print_prog f (fl, ll, fc, lc) color =
  let c = open_in f in
  (* got to line fl *)
  for _ = 1 to fl - 1 do
    ignore (input_line c)
  done;
  if fl = ll then (
    if fc <> lc then (
      eprintf "%d | %s@." fl (input_line c);
      eprintf "%s" (String.make (4 + fc) ' ');
      eprintf "@{<bold>@{<%s>%s@}@}@." color (String.make (lc - fc) '^')
    )
  ) else
    print_prog_lines c fl ll fc lc

let report file (fl, ll, fc, lc) =
  if fl <> ll then
    eprintf
      "@{<bold>File \"%s\", line %d-%d, characters %d-%d:@}\n"
      file fl ll fc lc
  else
    eprintf "@{<bold>File \"%s\", line %d, characters %d-%d:@}\n" file fl fc lc

let pose_lex ps pe =
  (ps.pos_lnum, pe.pos_lnum, ps.pos_cnum - ps.pos_bol, pe.pos_cnum - pe.pos_bol)

let warn_color = "fg_mag"

let err_color = "fg_red"

let print_info file pos color =
  report file pos;
  print_prog file pos color

(* Warning *)

let warn_not_unit file e =
  add_ansi_marking err_formatter;
  let pose = pose_lex e.loc.fc e.loc.lc in
  report file pose;
  print_prog file pose warn_color;
  eprintf
    "@{<bold>@{<fg_mag>Warning@}@}: this expression should have type unit.@."

(* Error *)

let print_missing_semi file l =
  print_info file (pose_lex l.fc l.lc) err_color;
  eprintf "@{<bold>@<fg_red>Error@}: Missing semicolon@."

let print_unclosed_error file l fc lc s =
  print_info file (l, l, fc, lc) err_color;
  eprintf "@{<bold>@{<fg_red>Error@}@}: lexical error: %s@." s

let print_syntax_err file lb =
  let pose = pose_lex (lexeme_start_p lb) (lexeme_end_p lb) in
  print_info file pose err_color;
  eprintf "@{<bold>@{<fg_red>Error@}@}: syntax error@."

let print_type_error file e s =
  let pose = pose_lex e.loc.fc e.loc.lc in
  print_info file pose err_color;
  eprintf "@{<bold>@{<fg_red>Error@}@}: @[%s@]@." s

(* types def *)

let print_type_constr_error file loc s =
  let pose = pose_lex loc.fc loc.lc in
  print_info file pose err_color;
  eprintf "@{<bold>@{<fg_red>Error@}@}: @[%s@]@." s

let print_error err file =
  add_ansi_marking err_formatter;
  match err with
  | Error.Unclosed (l, fc, lc, s) -> print_unclosed_error file l fc lc s
  | Error.Missing_semi l -> print_missing_semi file l
  | Error.Type_error (e, s) -> print_type_error file e s
  | Error.Type_def (loc, s) -> print_type_constr_error file loc s
  | _ -> ()

