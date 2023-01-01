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

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let prog = Parser.program Lexer.pattern lb in
    close_in c;
    (* ignore (Typechecker.type_prog prog); *)
    ignore (Typeinference.type_inference prog file);
    let output_value, mem = Interpreter.eval_prog prog in
    Value.print_value mem output_value;
    print_newline ()
  with
  | Parser.Error ->
      Errorcat.print_syntax_err file lb;
      exit 1
  | Error.Error (Invalid_argument s) ->
      eprintf "Exception: Invalide_argument \"%s\"\n" s;
      exit 1
  | Error.Error e ->
      Errorcat.print_error e file;
      exit 1
  | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
