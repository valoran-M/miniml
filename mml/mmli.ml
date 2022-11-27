open Lexing
open Format

let usage = "usage: ./mmli file.mml"
let spec = []

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".mml") then
      raise (Arg.Bad "no .mml extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with
  | Some f -> f
  | None ->
      Arg.usage spec usage;
      exit 1

let report (b, e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let prog = Parser.program Lexer.pattern lb in
    close_in c;
    Printf.printf "OK\n";
    ignore (Typechecker.type_prog prog);
    Printf.printf "OK\n";

    let output_value = Interpreter.eval_prog prog in
    Interpreter.print_value output_value
  with
  | Lexer.Lexing_error s ->
      report (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "lexical error: %s@." s;
      exit 1
  | Parser.Error ->
      report (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "syntax error@.";
      exit 1
  | Typechecker.Type_error s ->
      eprintf "type error: %s@." s;
      exit 1
  | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
