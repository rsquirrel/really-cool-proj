(* generator.ml *)
(* @authors: Yan Zou *)

open Bytecode
open Type
open Sast

module StringMap = Map.Make(String)

(* Index map for the three basic elements in TML *)
type environment = {
	func_index : int StringMap.t;
	global_index : int StringMap.t;
	tmember_index : int StringMap.t;
}

let string_of_type = function
	| Int -> "i"
	| Float -> "f"
	| Char -> "c"
	| String -> "s"
	| Boolean -> "b"
	| Tree_type tname -> "T"
	| Void -> "v"

let int_of_order = function
	| Preorder -> 0
	| Inorder -> 1
	| Postorder -> 2
	| Levelorder -> 3

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
	| Father -> "Fat"
	| Deg_a -> "Deg"
	| Dot -> "Val"
	| Hsh -> "Num"
	| Child -> "Chd"

(* convert a string to a list of ascii codes *)
let string_of_string s = 
	let len = String.length s in
	let rec toasc result i =
		if i = len then result
		else
				let result =
					(result ^ " " ^ (string_of_int (Char.code (String.get s i))))
				in toasc result (i + 1)
	in toasc "" 0

(* convert bytecode to string *)
let string_of_bytecode = function
	| Glb i -> ("Glb " ^ (string_of_int i))
	| Psi i -> ("Psi " ^ (string_of_int i))
	| Psf f -> ("Psf " ^ (string_of_float f))
	| Psc c -> ("Psc " ^ (string_of_int (Char.code c)))
	| Pss s -> ("Pss" ^ (string_of_string s))
	| Psb b -> ("Psb " ^ (string_of_bool b))
	| Pst -> ("Pst")
	| Pop i -> ("Pop " ^ (string_of_int i))
	| Uop (un_op, t) ->
			((if un_op = Sub then "Neg" else (string_of_op un_op)) ^
					" " ^ (string_of_type t))
	| Bin (bin_op, t) -> ((string_of_op bin_op) ^ " " ^ (string_of_type t))
	| Lod i -> ("Lod " ^ (string_of_int i))
	| Str i -> ("Str " ^ (string_of_int i))
	| Lfp i -> ("Lfp " ^ (string_of_int i))
	| Sfp i -> ("Sfp " ^ (string_of_int i))
	| Jsr i -> ("Jsr " ^ (string_of_int i))
	| Ent i -> ("Ent " ^ (string_of_int i))
	| Ret i -> ("Ret " ^ (string_of_int i))
	| Beq i -> ("Beq " ^ (string_of_int i))
	| Bne i -> ("Bne " ^ (string_of_int i))
	| Bra i -> ("Bra " ^ (string_of_int i))
	| Alc i -> ("Alc " ^ (string_of_int i))
	| Fld t -> ("Fld " ^ (string_of_type t))
	| Sfd i -> ("Sfd " ^ (string_of_int i))
	| Scd -> "Scd"
	| Nxt i -> ("Nxt " ^ (string_of_int i))
	| Hlt -> "Hlt"

let init_value t = function
	| Some(e) -> e
	| None ->
			let lit = match t with
				| Int -> IntLit(0)
				| Float -> FloatLit(0.0)
				| Char -> CharLit('\000')
				| String -> StringLit("")
				| Boolean -> BoolLit(false)
				| Tree_type tname -> TreeLit
				| Void -> IntLit(0) (* "Variables with type void shouldn't exist" *)
			in Literal(lit), t
							
(* add the names in the list to the map starting from start_index *)
let add_to_map list map start_index =
	snd (List.fold_left (fun (i, map) name ->
		(*print_endline (name ^ ": " ^ (string_of_int i)); (* debug *) *)
		(i + 1, StringMap.add name i map)) (start_index, map) list)

let translate out_filename program =
	(* index map for all globals - don't vary *)
	let glob_index =
		let gname_list =
			List.map (fun (_, n, _) -> n) program.globals
		in
		add_to_map gname_list StringMap.empty 0
	in
	(* index map for all functions - don't vary *)
	let func_index = 
		let fname_list =
			List.map (fun f -> f.fname) program.functions
		in
		(* tree allocation function names are same with the tree type names *)
		(* after type checking, they can't be the same with other functions *)
		let tname_list =
			List.map (fun t -> t.typename) program.treetypes
		in
		let built_in = (* built-in functions *)
			add_to_map ["print"] StringMap.empty (-1)
		in
		let func_index_without_trees =
			add_to_map fname_list built_in 0
		in (* append tree alloc functions to the end of funcs *)
		add_to_map tname_list func_index_without_trees
			(List.length fname_list) (* starting offset of tree alloc functions *)
	in
	let tmember_index =
		let add_to_map_2 map t = (* for trees *)
			let tree_name = t.typename in
			let member_list = (* add tree_name before member names *)
				(* this avoids conflicts because "." is not allowed in identifiers *)
				List.map (fun (_, name, _) -> tree_name ^ "." ^ name) t.members
			in
			add_to_map member_list map 0
		in
		List.fold_left add_to_map_2 StringMap.empty program.treetypes
	in
	(* translate all the stmts in a block, local_index will not vary *)
	let rec block local_index next_index num_params sl =	
		(* common function for Binop and Assign in generating expr *)
		let get_tmember_index e1 e2 =
			match (snd e1) with (* get tree type name *)
				| Tree_type tree_name -> 
						(match e2 with
							| Id id, _ -> (* tree_name.id *)
									(let member_id = tree_name ^ "." ^ id in
									try StringMap.find member_id tmember_index
									with Not_found -> 
										raise (Failure ("tree member " ^
											member_id ^ " not found")))
							| _ -> 
									raise (Failure ("The right operand of operator " ^
										". can only be an identifier")))
				| _ ->
						raise (Failure ("The left operand of operator " ^
										". can only be tree type"))
		in
		(* Evaluate expression, the result is on top of the stack *)
		let rec expr = function
			| Literal lit, _ -> ( match lit with
				| IntLit i -> [Psi i]
				| FloatLit f -> [Psf f]
				| CharLit c -> [Psc c]
				| BoolLit b -> [Psb b]
				| StringLit s -> [Pss s]
				| TreeLit -> [Pst] )
  		| Id id, _ -> (* the ID of tree member won't get here *)
					( try [Lfp (StringMap.find id local_index)]
						with Not_found ->
							try [Lod (StringMap.find id glob_index)]
							with Not_found ->
								raise (Failure ("Variable " ^ id ^ " not found")))
  		| Binop (e1, bin_op, e2), t ->
					let be2 =
						if bin_op = Dot then
							[Psi (get_tmember_index e1 e2)]
						else expr e2
					in
					(expr e1) @ be2 @ [Bin (bin_op, (snd e1))]
			| Assign (e1, e2), _ ->
					(* left value for assignment, get the address or use Sfp/Str/Sfd/Scd *)
					(match e1 with
						| Id id, t ->
								let lvalue_bytecode =
									( try [Sfp (StringMap.find id local_index)]
										with Not_found ->
											try [Str (StringMap.find id glob_index)]
											with Not_found ->
												raise (Failure ("Variable " ^ id ^ " not found")))
								in (expr e2) @ lvalue_bytecode
						| Binop (e11, bin_op, e12), t ->
								( match bin_op with
										| Dot -> let i = get_tmember_index e11 e12 in
												(expr e11) @ (expr e2) @ [Sfd i]
										| Child -> 
												(expr e11) @ (expr e12) @ (expr e2) @ [Scd]
										| _ -> raise (Failure "Illegal left value") )
						| _ -> raise (Failure "Illegal left value"))
	 		| Call (func_name, params), t -> 
					if (func_name = "print") then
						if (List.length params > 0) then
							(* expr (List.hd params) @ [Jsr (-1)] @
							List.concat (List.map (fun e -> [Pop 1] @ (expr e) @
																				[Jsr (-1)]) (List.tl params))*)
							(List.concat (List.map (fun e -> (* print one by one *)
								(expr e) @ [Jsr (-1); Pop 1]) params)) @
							[Psi 0] (* return value of print *)
						else []
					else if (func_name = "alloc") then 
						let alloc e = (* alloc one tree *)
							match e with (* alloc only when the parameters are tree-type left values *)
								| Id _, Tree_type tree_name
								| Binop (_, Dot, _), Tree_type tree_name
								| Binop (_, Child, _), Tree_type tree_name -> 
										let ea =
											Assign (e, (Call (tree_name, [e]),
												Tree_type tree_name)), Tree_type tree_name
										in
										expr ea @ [Pop 1]
								| _ -> (* else, do nothing *)
										raise (Failure "illegal alloc parameters")
						in
						if (List.length params > 0) then (* alloc one by one *)
							(List.concat (List.map (fun e -> alloc e) params)) @
							[Psi 0] (* void return of alloc *)
						else []
					else
						List.concat (List.map expr params) @
						(try [Jsr (StringMap.find func_name func_index)]
						 with Not_found ->
							raise (Failure ("Function " ^ func_name ^ " not found")))
	  	| Uniop (un_op, e), t ->
					if un_op = Add then (expr e) else ((expr e) @ [Uop (un_op, t)])
	  	| Conn (e, el), _ -> (* add to children one by one *)
					let (_, add_children_byte) =
						List.fold_left (fun (i, bl) ec ->
							(i + 1, bl @ [Psi i] @ (expr ec) @ [Scd])) (0, []) el
					in
					expr e @ add_children_byte
			| Noexpr, _ -> [Psi 0] (* void return of a function *)
		in
		let loop_control sl continue_offset break_offset = 
			let (_, new_sl) =
				List.fold_left (fun (i, l) -> function
					(* the Jsr -3 and -4 here are just stubs which will be changed later *)
					| Jsr (-3) -> (i + 1, (Bra (break_offset - i))::l) (* Break *)
					| Jsr (-4) -> (i + 1, (Bra (continue_offset - i))::l) (* Continue *)
					| _ as s -> (i + 1, s::l)) (0, []) sl
			in List.rev new_sl
		in
		let rec stmt = function 
			| Block (sl, vl) -> (* need new locals *)
					let vname_list = List.map (fun (_, n) -> n) vl in
					let new_local_index =
						add_to_map vname_list local_index next_index
					in
					let num_locals = List.length vl in
					(block new_local_index (next_index + num_locals) num_params sl) @
					( if num_locals > 0 then
							[Pop num_locals] (* pop out the locals in the block to save space *)
						else [] )
			| Expr e -> expr e @ [Pop 1]
			| Return e -> expr e @ [Ret num_params]
			| If (e, s1, s2) ->
					let r1 = (stmt s1) and r2 = (stmt s2) in
					expr e @ [Beq ((List.length r1) + 2)] @ r1 @
					[Bra ((List.length r2) + 1)] @ r2
			| Foreach (id, e, order, s) ->
					let new_local_index =
						(* print_endline (id ^ ": " ^ (string_of_int next_index)); (* debug *)*)
						StringMap.add id next_index local_index
					in
					let rs = (* regard the foreach statement as a block *)
						(* the id can only be used inside foreach *)
						(* index moves 2 forward because of id and order *)
						block new_local_index (next_index + 2) num_params [s] (* double blocks *)
					in
					let continue_offset = List.length rs in
					let break_offset = continue_offset + 1 in
					expr e @ (* push the root node onto the stack *)
					[Psi (int_of_order order)] @ (* traverse order *)
					(* [Psi (List.length bytecodes)] @ (* number of lines in foreach *) *)
					[Jsr (-2)] @
					(loop_control rs continue_offset break_offset) @
					[Nxt (- List.length rs)] @ [Pop 2] 
					(* in order to make variables in the right order on the stack *)
			| For (e1, e2, e3, s) -> 
					(* stmt (Block([Expr(e1); While(e2, Block([s; Expr(e3)]))])) *)
					let re1 = stmt (Expr e1) and re2 = expr e2 and
							re3 = stmt (Expr e3) and rs = stmt s
					in
					let continue_offset = List.length rs in
					let check_offset = continue_offset + (List.length re3) in
					let break_offset = check_offset + (List.length re2) + 1 in
					re1 @ [Bra (check_offset + 1)] @
					(loop_control rs continue_offset break_offset) @
					re3 @ re2 @ [Bne (-break_offset + 1)]
			| Do (s, e) ->
					let rs = stmt s and re = expr e in
					let continue_offset = List.length rs in
					let break_offset = continue_offset + (List.length re) + 1 in
					(loop_control rs continue_offset break_offset) @
					re @ [Bne (-break_offset + 1)]
			| While (e, s) ->
					let re = expr e and rs = stmt s in
					let continue_offset = List.length rs in
					let break_offset = continue_offset + (List.length re) + 1 in
					[Bra (continue_offset + 1)] @
					(loop_control rs continue_offset break_offset) @
					re @ [Bne (-break_offset + 1)]
			(* break and continue are just stubs *)
			(* in case that local variables are declared in the loop scope *)
			(* we need to record the current number of local variables in the stub *)
			(* they will be adjusted later so that break or continue *)
			(* will pop out all local variables in the loop scope before branching *)
			| Break i -> [Pop i; Jsr (-3)] (* stub, Indicate it's a break *)
			| Continue i -> [Pop i; Jsr (-4)] (* Indicate it's a continue *)
			| Vardecl (t, name, init) -> expr (init_value t init) (* leave it on the stack *)
				(* The stack space allocated for the new locals move along with the sp *)
				(* The locals will be consecutive in space as var_decl is a complete *)
				(* statement. There shouldn't be any intermediate result on the stack *)
				(* between statements, which only appears when evaluating expressions. *)
			| Empty -> []
		in
		List.concat (List.map stmt sl)
	in
	let data =
		(* convert all the global declarations into a block of assignments *)
		let assign_globals = 
			List.map (fun (t, name, init) ->
				Expr(Assign((Id(name), t), init_value t init), t)) program.globals
		in
		[Glb (List.length assign_globals)] @
		block StringMap.empty 0 0 assign_globals @
		try
			[Jsr (StringMap.find "main" func_index); Hlt]
		with Not_found -> 
			raise (Failure "main function not found!")
	in
	let text = 
		(* convert function bodies into bytecodes *)
		let trans_func f =
			let param_names = List.map snd f.params in (* get the names *)
			let num_params = List.length param_names in
			let param_index = (* the last parameter will have index -2 *)
				add_to_map param_names StringMap.empty (-1 - num_params)
			in
			let local_names = List.map snd f.locals in (* get the names *)
			let num_locals = List.length local_names in
			let local_index = (* the first local variable will have index 1 *)
				add_to_map local_names param_index 1
			in
			let bodycodes =
				block local_index (num_locals + 1) num_params f.body
			in
			(* check if there are break or continue statements not adjusted *)
			List.iter (function
				| Jsr (-3) -> raise (Failure "break not in loops")
				| Jsr (-4) -> raise (Failure "continue not in loops")
				| b -> ()) bodycodes;
			[Ent num_locals] @ bodycodes @
			(match f.return_type with
				| Int -> [Psi 0]
				| Float -> [Psf 0.0]
				| Char -> [Psc '\000']
				| String -> [Pss ""]
				| Boolean -> [Psb false]
				| Tree_type tname -> [Pst]
				| Void -> [Psi 0]) @
			[Ret num_params]
		in
		(* convert tree definitions into alloc functions *)
		let trans_trees treetype =
			let init_members = (* generate the member initialization statements *)
				List.map (fun (t, name, init) ->
					Expr(Assign((Binop((Id("."), Tree_type(treetype.typename)),
						Dot, (Id(name), t)), t), init_value t init), t))
					treetype.members
			in
			let tree_init_map = (* fake map only containing the tree itself *)
				StringMap.add "." (-2) StringMap.empty
			in
			[Ent 0; Lfp (-2); Alc treetype.degree] @ (* get the tree - the only parameter *)
			List.map (fun (t, _, _) -> Fld t) treetype.members @ [Sfp (-2); Pop 1] @
			block tree_init_map 0 1 init_members @
			[Lfp (-2); Ret 1] (* return the tree *)
		in
		let func_bodies =
			(List.map trans_func program.functions) @
			(List.map trans_trees program.treetypes)
		in
		let (_, func_offsets) = (* generate the function offsets as a list *)
				List.fold_left (fun (i, offsets) body ->
						(* each step plus the number of byte statements *)
						((List.length body) + i, i::offsets))
					(* start from the end of global variable initialization *)
					((List.length data), []) func_bodies
					(* +1 is for Glb *)
		in
		let array_offsets = Array.of_list (List.rev func_offsets) in
		List.map (function
			| Jsr i when i >= 0 -> Jsr array_offsets.(i)
			| _ as s -> s)
			(List.concat (data::func_bodies))
	in
	let fout = open_out out_filename in
		ignore ( List.fold_left (fun i b ->
				(* output line number for debug *)
				output_string fout ((string_of_int i) ^ "\t" ^ (string_of_bytecode b) ^ "\n");
				(i + 1)) 0 text
		);
		close_out fout