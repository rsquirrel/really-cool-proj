(* tml.ml *)
(* @authors: Yan Zou *)

let lexbuf = Lexing.from_channel stdin in
let program = Analyzer.check (Parser.program Scanner.token lexbuf) in
	Generator.translate program
	