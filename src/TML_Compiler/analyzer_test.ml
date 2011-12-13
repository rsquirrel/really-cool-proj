open Sast;;

let lexbuf = Lexing.from_channel stdin in
let program = Analyzer.check (Parser.program Scanner.token lexbuf) in
	List.iter (fun (_, name, _) -> print_endline name) program.globals