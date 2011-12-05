(* tml.ml *)
(* @authors: Yan Zou *)

let lexbuf = Lexing.from_channel stdin in
let program = Parser.program Scanner.token lexbuf in
	Compiler.translate program
	