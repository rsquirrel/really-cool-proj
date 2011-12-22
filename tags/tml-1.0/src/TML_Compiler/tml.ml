(* tml.ml *)
(* @authors: Yan Zou *)

if (Array.length Sys.argv < 2) then
	print_endline "Usage: tmlc <file_name>"
else
	let in_filename = Sys.argv.(1) in
	let out_filename =
		let slash_pos = (* the beginning of filename *)
			try String.rindex in_filename '/' 
			with Not_found -> -1
		in
		let dot_pos = (* the end of filename *)
			try String.rindex in_filename '.'
			with Not_found -> String.length in_filename
		in
		let dot_pos = (* end should > begin, otherwise dot is for directory *)
			if dot_pos < slash_pos then
				(String.length in_filename)
			else
				dot_pos
		in
		(String.sub in_filename 0 dot_pos) ^ ".tmb"
	in
	let fin = open_in in_filename in
	let lexbuf = Lexing.from_channel fin in
	let program = Analyzer.check (Parser.program Scanner.token lexbuf) in
		ignore(close_in fin); Generator.translate out_filename program