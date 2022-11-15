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
  let prog = Parser.program Lexer.pattern lb in
  close_in c;
  let output_file = file ^ ".cat" in
  let out = open_out output_file in
  let outf = formatter_of_out_channel out in
  Pparser.print_prog outf prog;
  close_out out
