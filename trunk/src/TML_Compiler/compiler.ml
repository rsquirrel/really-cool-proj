(*let lexbuf = Lexing.from_channel stdin in
	Parser.program Scanner.token lexbuf*)
	
(* compiler.ml *)
(* @authors: Yan Zou *)

open Ast
open Bytecode

module StringMap = Map.Make(String)

(* Index map for the three basic elements in TML *)
type environment = {
		func_index : int StringMap.t;
		global_index : int StringMap.t;
		tree_index : int StringMap.t;
	}

(* translate operation to string for writing bytecodes to file *)
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

let int_of_bool = function
	| true -> 1
	| false -> 0

let translate program =
	let (global_init, func_bodies) =
	  (* generate index and store it in map *)
		(* The first one will have index "start", and the following *)
		(* index is incremented by 1 one after another. *)
		let rec gen_index map start = function
			| [] -> map
			| hd::tl -> let new_map =
					StringMap.add (snd hd) start map in
					gen_index new_map (start + 1) tl
		in
		(* translate expressions, used by both trans_global and trans_stmt *)
		let trans_expr env locals e =
			(* left value for assignment, get the address or use Sfp/Str *)
			let rec lvalue = function
				| Id id ->
					(try [Sfp (StringMap.find id locals)]
					 with Not_found ->
						try [Str (StringMap.find id env.global_index)]
						with Not_found ->
							raise (Failure ("Variable " ^ id ^ "not found")))
				| Binop (e1, bin_op, e2) ->
					( match bin_op with
							| Dot -> []
							| Child -> []
							| _ -> raise (Failure "Illegal left value") )
				| _ -> raise (Failure "Illegal left value")
			in
			(* Evaluate expression, the result is on top of the stack *)
			let rec expr = function
				| Literal lit -> ( match lit with
					| IntLit i -> [Psh i]
					| FloatLit f -> [Psh (int_of_float f)]
					| CharLit c -> [Psh (int_of_char c)]
					| BoolLit b -> [Psh (int_of_bool b)]
					| StringLit str -> [Psh 2000(*(int_of_string s)*)]
					| TreeLit -> [Psh 10000] )
  			| Id id ->
					(try [Lfp (StringMap.find id locals)]
					 with Not_found ->
						try [Lod (StringMap.find id env.global_index)]
						with Not_found ->
							raise (Failure ("Variable " ^ id ^ "not found")))
  			| Binop (e1, bin_op, e2) ->
						(expr e1) @ (expr e2) @ [Bin bin_op]
				| Assign (e1, e2) -> (expr e2) @ (lvalue e1)
  			| Call (func_name, params) -> 
						List.concat (List.map expr (List.rev params)) @
						(try [Jsr (StringMap.find func_name env.func_index)]
						 with Not_found ->
							raise (Failure ("Function" ^ func_name ^ "not found")))
  			| Uniop (un_op, e) -> (expr e) @ [Bin un_op]
  			| Conn (e, el) -> [] (* TODO *)
				| Noexpr -> [] (* TODO: What is this? *)
			in expr e
		in
		(* add to locals according to the init_list *)
		let trans_globals env next_index var_decl =
			let rec add_global globals next_index byte_list = function
				| [] -> (globals, List.rev byte_list)
				| hd::tl ->
					let (name, init) = match hd with
						| WithInit (name, e) ->
								let new_env = {env with global_index = globals} in
								(name, (trans_expr new_env StringMap.empty e) @
												[Str next_index; Pop])
						| WithoutInit name -> (name, [])
						(* Each local is allocated a unit on the stack *)
					in
						add_global (StringMap.add name next_index globals)
											 (next_index + 1) (init::byte_list) tl
			in add_global env.global_index next_index [] (snd var_decl)
		in
		(* translate one function body and return the list of bytecodes *)
		let trans_func env func_decl =
			let num_params = List.length func_decl.params in
			(* generate repeat bytecode statements according to times *)
			let rec repeat times byte_stmt =
				if (times == 0) then []
				else byte_stmt::(repeat (times - 1) byte_stmt)
			in
			(* translate one statement/block with certain locals *)
			let rec trans_stmt locals next_index target =
				(* add to locals according to the init_list *)
				let rec add_local locals next_index byte_list = function
					| [] -> (locals, next_index, byte_list)
					| hd::tl ->
						let (name, init) = match hd with
							| WithInit (name, e) -> (name, trans_expr env locals e)
							| WithoutInit name -> (name, [Psh 0])
							(* Each local is allocated a unit on the stack *)
						in
							add_local (StringMap.add name next_index locals)
												(next_index + 1) (byte_list @ init) tl
				in
				let rec loop_control sl continue_offset break_offset = 
					let (_, new_sl) =
						List.fold_left (fun (i, l) -> function
							| Jsr (-3) -> (i + 1, (Bra (break_offset - i))::l) (* Break *)
							| Jsr (-4) -> (i + 1, (Bra (continue_offset - i))::l) (* Continue *)
							| _ as s -> (i + 1, s::l)) (0, []) sl
					in List.rev new_sl
				in
				(* create the new locals for the coming stmt list *)
				(* call trans_stmt instead of stmt after adding new locals *)
				let rec stmts locals next_index = function
					| [] -> []
					| Vardecl var_decl::tl -> (* change the locals and next_index *)
							(* The space allocated for the new locals move along with the sp *)
							(* The locals must be consecutive in space as var_decl is a complete *)
							(* statement. There should be any intermediate result on the stack *)
							(* between statements, which only appears when evaluating expressions. *)
							let (new_locals, new_next_index, new_byte_list) =
								add_local locals next_index [] (snd var_decl)
							in
							new_byte_list @ (* first allocate the space for the new locals *)
							(* following statements with new locals *)
							(stmts new_locals new_next_index tl) @
							(* get rid of the locals after finishing the statement list *)
							(repeat (new_next_index - next_index) Pop)
					| hd::tl -> (trans_stmt locals next_index hd) @ (* use new locals *)
											(stmts locals next_index tl)
				in
				(* recursively deal with one statement with locals unchanged *)
				let rec stmt = function 
					| Block sl -> stmts locals next_index sl (* need new locals *)
							(* let (new_next_index, byte_list) = stmts locals next_index [] sl
							in byte_list @ (repeat (new_next_index - next_index) Pop) *)
					| Expr e -> trans_expr env locals e @ [Pop]
					| Return e -> trans_expr env locals e @ [Ret num_params]
					| ReturnVoid -> [Ret num_params] (* TODO: same with Return first *)
					| If (e, s1, s2) ->
							let r1 = (stmt s1) and r2 = (stmt s2) in
							trans_expr env locals e @ [Beq ((List.length r1) + 2)] @ r1 @
							[Bra ((List.length r2) + 1)] @ r2
					| Foreach (id, e, order, s) -> []
					| For (e1, e2, e3, s) -> 
							(* stmt (Block([Expr(e1); While(e2, Block([s; Expr(e3)]))])) *)
							let re1 = stmt (Expr e1) and re2 = trans_expr env locals e2 and
									re3 = stmt (Expr e3) and rs = stmt s
							in
							let continue_offset = List.length rs in
							let check_offset = continue_offset + (List.length re3) in
							let break_offset = check_offset + (List.length re2) + 1 in
							re1 @ [Bra (check_offset + 1)] @
							(loop_control rs continue_offset break_offset) @
							re3 @ re2 @ [Bne (-break_offset + 1)]
					| Do (s, e) ->
							let rs = stmt s and re = trans_expr env locals e in
							let continue_offset = List.length rs in
							let break_offset = continue_offset + (List.length re) + 1 in
							(loop_control rs continue_offset break_offset) @
							re @ [Bne (-break_offset + 1)]
					| While (e, s) ->
							let re = trans_expr env locals e and rs = stmt s in
							let continue_offset = List.length rs in
							let break_offset = continue_offset + (List.length re) + 1 in
							[Bra (continue_offset + 1)] @
							(loop_control rs continue_offset break_offset) @
							re @ [Bne (-break_offset + 1)]
					| Break -> [Jsr (-3)] (* Indicate it's a break *)
					| Continue -> [Jsr (-4)] (* Indicate it's a continue *)
					| Vardecl var_decl -> (* separately dealt with *)
							raise (Failure "Variable declaration in wrong position!") 
					| Empty -> []
				in stmt target
			in
			(* initialize locals with function parameters by negative index *)
			let init_locals = 
				gen_index StringMap.empty (-num_params) func_decl.params
			in
			[Ent 1] @
			(trans_stmt init_locals 1 (Block func_decl.body)) @
			[Ret num_params]
		in
		let init_env = {
				func_index =
					gen_index StringMap.empty (-2)
						[(Void, "alloc"); (Void, "print")];
				global_index = StringMap.empty;
				tree_index = StringMap.empty;
			} in
		(* go through all the basic element in the program and classify *)
		(* them into globals, functions and trees respectively *)
		let rec classify global_init func_bodies env = function
			| [] ->
					let entry_func = 
						(try [Jsr (StringMap.find "main" env.func_index); Hlt]
						 with Not_found -> raise (Failure "No main function!"))
					in
						(global_init, entry_func::(List.rev func_bodies))
			| (Globalvar var_decl)::tl -> (* put it into the loop *)
					let (new_index, new_init) =
						trans_globals env (List.length global_init) var_decl
					in
					let new_env = {env with global_index = new_index} in
					classify (global_init @ new_init) func_bodies new_env tl
			| (Funcdef func_decl)::tl ->
					let new_env = { env with func_index = 
						StringMap.add func_decl.fname
							(List.length func_bodies + 1) env.func_index }
							(* +1 to leave a position for entry_func *)
					in
					classify global_init ((trans_func env func_decl)::
																func_bodies) new_env tl
			| (Treedef tree_def)::tl ->
					classify global_init func_bodies env tl
		in classify [] [] init_env (List.rev program)
	in
	(* deal with the Jsr offsets for each function *)
	let text = 
		let global_init = List.concat global_init in
		let (_, func_offsets) = (* generate the function offsets as a list *)
				List.fold_left (fun (i, offsets) bl ->
						(* each step plus the number of byte statements *)
						((List.length bl) + i, i::offsets))
					(* start from the end of global variable initialization *)
					((List.length global_init) + 1, []) func_bodies
					(* +1 is for Glb *)
		in
		let array_offsets = Array.of_list (List.rev func_offsets) in
		let func_bodies =
			List.map (function
				| Jsr i when i >= 0 -> Jsr array_offsets.(i)
				| _ as s -> s)
				(List.concat func_bodies)
		in
		[Glb (List.length global_init)] @ global_init @ func_bodies
	in
	(* output to the bytecode file *)
	let fout = open_out "test.byte" in
		(* output_string fout ".data\n";
		ignore (List.map (fun str_data -> output_string fout (str_data ^ "\n")) data);
		output_string fout ".text\n"; *)
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
				| Fld i -> output_endline ("Fld " ^ (string_of_int i))
				| Glb i -> output_endline ("Glb " ^ (string_of_int i))) text);
		close_out fout
