(*let lexbuf = Lexing.from_channel stdin in
	Parser.program Scanner.token lexbuf*)
	
(* compiler.ml *)
(* @authors: Yan Zou *)

open Ast
open Bytecode

module StringMap = Map.Make(String)

type env = {
		func_index : int StringMap.t;
		global_index : int StringMap.t;
		local_index : int StringMap.t;
	}

let string_of_op = function
	  Add -> "Add"
	| Sub -> "Sub"
	| Mult -> "Mul" 
	| Div -> "Div"
	| Equal -> "Eq"
	| Neq -> "Neq"
	| Less_than -> "Lt"
	| Leq -> "Leq"
	| Greater_than -> "Gt"
	| Or -> "Or"
	| And -> "And"
	| Not -> "Not"
	| Geq -> "Geq"
	| Mod -> "Mod"
	| Dollar -> "Cln"
	| At -> "At"
	| Deg_a -> "Deg"
	| Dot -> "Val"
	| Hsh -> "Num"
	| Child -> "Chd"
	| Assign -> "Asn"

let translate program =
	let (data, func_bodies) = 
		let trans_func env func_decl =
			let num_params = List.length func_decl.params in
			let rec expr = function
				| Literal i -> [Psh 0]
  			| Id id -> [Psh (-1)]
  			| Binop (e1, bin_op, e2) ->
						(expr e1) @ (expr e2) @ [Bin bin_op]
  			| Call (func_name, params) -> 
						List.concat (List.map expr (List.rev params)) @
						[Jsr (-1)]
  			| Uniop (un_op, e) -> (expr e) @ [Bin un_op]
  			| Conn (e, el) -> []
				| Noexpr -> []
			in
			let rec stmt = function
				| Block sl -> List.concat (List.map stmt sl)
				| Expr e -> expr e @ [Pop]
				| Return e -> expr e @ [Ret num_params]
				| ReturnVoid -> [Ret num_params]
				| If (e, s1, s2) ->
						let r1 = stmt s1 and r2 = stmt s2 in
						expr e @ [Beq ((List.length r1) + 2)] @ r1 @
						[Bra ((List.length r2) + 1)] @ r2
				| Foreach (id, e, order, s) -> []
				| For (e1, e2, e3, s) -> 
						stmt (Block([Expr(e1); While(e2, Block([s; Expr(e3)]))]))
				| Do (s, e) ->
						let rs = stmt s and re = expr e in
						rs @ re @ [Bne (- (List.length rs) - (List.length re))]
				| While (e, s) ->
						let re = expr e and rs = stmt s in
						[Bra ((List.length rs) + 1)] @ rs @ re @
						[Bne (- (List.length re) - (List.length rs))]
				| Break -> [Bra 1]
				| Continue -> [Bra 1]
				| Vardecl var_decl -> []
				| Empty -> []
			in
			[Ent 1] @
			stmt (Block func_decl.body) @
			[Ret num_params]
		in
		let env = {
				func_index = StringMap.empty;
				global_index = StringMap.empty;
				local_index = StringMap.empty;
			} in
		let rec classify gen_data gen_bodies = function
			| [] -> (List.rev gen_data, List.rev gen_bodies)
			| (Globalvar var_decl)::tl ->
					classify ("g"::gen_data) gen_bodies tl
			| (Funcdef func_decl)::tl ->
					classify gen_data ((trans_func env func_decl)::gen_bodies) tl
			| (Treedef tree_def)::tl ->
					classify gen_data gen_bodies tl
		in classify [] [] program
	in
	let text = List.concat func_bodies in
	let fout = open_out "test.byte" in
		output_string fout ".data\n";
		ignore (List.map (fun str_data -> output_string fout (str_data ^ "\n")) data);
		output_string fout ".text\n";
		ignore (
			let output_endline = fun str ->
				output_string fout (str ^ "\n")
			in
			List.map (function
				| Psh i -> output_endline ("Psh " ^ (string_of_int i))
				| Pop -> output_endline "Pop"
				| Uop un_op -> output_endline (string_of_op un_op)
				| Bin bin_op -> output_endline (string_of_op bin_op)
				| Lod i -> output_endline ("Lod " ^ (string_of_int i))
				| Str i -> output_endline ("Str " ^ (string_of_int i))
				| Lfp i -> output_endline ("Lfp " ^ (string_of_int i))
				| Sfp i -> output_endline ("Sfp " ^ (string_of_int i))
				| Jsr i -> output_endline ("Jsr " ^ (string_of_int i))
				| Ent i -> output_endline ("Ent " ^ (string_of_int i))
				| Ret i -> output_endline ("Ret " ^ (string_of_int i))
				| Beq i -> output_endline ("Beq " ^ (string_of_int i))
				| Bne i -> output_endline ("Bne " ^ (string_of_int i))
				| Bra i -> output_endline ("Bra " ^ (string_of_int i))
				| Hlt -> output_endline "Hlt"
				| Fld i -> output_endline ("Fld " ^ (string_of_int i))) text);
		close_out fout
