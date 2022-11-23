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
  let prog = Parser.program Lexer.pattern lb in
  close_in c;
  let output_value = Interpreter.eval_prog prog in
  Interpreter.print_value output_value
