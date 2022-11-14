open Mml

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

let rec print_prog = function
  | Int i -> string_of_int i
  | Bop (Add, x, y) -> Printf.sprintf "(%s + %s)" (print_prog x) (print_prog y)
  | Bop (Mul, x, y) -> Printf.sprintf "(%s * %s)" (print_prog x) (print_prog y)

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  let prog = Parser.program Lexer.pattern lb in
  close_in c;
  Printf.printf "%s\n" (print_prog prog.code)
