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
	tree_index : int StringMap.t;
}

let string_of_type = function
	| Int -> "int"
	| Float -> "float"
	| Char -> "char"
	| String -> "string"
	| Boolean -> "bool"
	| Tree_type tname -> ("Tree(" ^ tname ^ ")")
	| Void -> "void"

let string_of_order = function
	| Preorder -> "preorder"
	| Inorder -> "inorder"
	| Postorder -> "postorder"
	| Levelorder -> "levelorder"

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

let string_of_bytecode = function
	| Psi i -> ("Psi " ^ (string_of_int i))
	| Psf f -> ("Psf " ^ (string_of_float f))
	| Psc c -> ("Psc " ^ (String.make 1 c))
	| Pss s -> ("Pss " ^ s)
	| Psb b -> ("Psb " ^ (string_of_bool b))
	| Pst -> ("Pst")
	| Pop i -> ("Pop " ^ (string_of_int i))
	| Uop (un_op, t) -> ((string_of_op un_op) ^ " " ^ (string_of_type t))
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
	| Hlt -> "Hlt"
	| Fld i -> ("Fld " ^ (string_of_int i))
	| Glb i -> ("Glb " ^ (string_of_int i))

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
				| Void -> raise (Failure ("Variables with type void shouldn't exist"))
			in Literal(lit), t
							
(* add the names in the list to the map starting from start_index *)
let add_to_map list map start_index =
	snd (List.fold_left (fun (i, map) name ->
		(i + 1, StringMap.add name i map)) (start_index, map) list)

let translate out_filename program =
	let gname_list = List.map (fun (_, n, _) -> n) program.globals in
	let fname_list = List.map (fun f -> f.fname) program.functions in
	let glob_index = add_to_map gname_list StringMap.empty 0 in
	let built_in = add_to_map ["alloc"; "print"] StringMap.empty (-2) in
	let func_index = add_to_map fname_list built_in 0 in
	(* translate all the stmts in a block, local_index will not vary *)
	let rec block local_index next_index num_params sl =	
		(* Evaluate expression, the result is on top of the stack *)
		let rec expr = function
			| Literal lit, _ -> ( match lit with
				| IntLit i -> [Psi i]
				| FloatLit f -> [Psf f]
				| CharLit c -> [Psc c]
				| BoolLit b -> [Psb b]
				| StringLit s -> [Pss s]
				| TreeLit -> [Pst] )
  		| Id id, _ ->
					( try [Lfp (StringMap.find id local_index)]
						with Not_found ->
							try [Lod (StringMap.find id glob_index)]
							with Not_found ->
								raise (Failure ("Variable " ^ id ^ " not found")))
  		| Binop (e1, bin_op, e2), t ->
					(expr e1) @ (expr e2) @ [Bin (bin_op, t)]
			| Assign (e1, e2), _ ->
					(* left value for assignment, get the address or use Sfp/Str *)
					let lvalue = function
						| Id id, t ->
								( try [Sfp (StringMap.find id local_index)]
									with Not_found ->
										try [Str (StringMap.find id glob_index)]
										with Not_found ->
											raise (Failure ("Variable " ^ id ^ "not found")))
						| Binop (e1, bin_op, e2), t ->
								( match bin_op with
										| Dot -> []
										| Child -> []
										| _ -> raise (Failure "Illegal left value") )
						| _ -> raise (Failure "Illegal left value")
					in (expr e2) @ (lvalue e1)
	 		| Call (func_name, params), _ -> 
					List.concat (List.map expr (List.rev params)) @
					(try [Jsr (StringMap.find func_name func_index)]
					 with Not_found ->
						raise (Failure ("Function " ^ func_name ^ " not found")))
	  	| Uniop (un_op, e), t -> (expr e) @ [Uop (un_op, t)]
	  	| Conn (e, el), _ -> [] (* TODO *)
			| Noexpr, _ -> [] (* TODO: What is this? *)
		in
		let loop_control sl continue_offset break_offset = 
			let (_, new_sl) =
				List.fold_left (fun (i, l) -> function
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
			| Foreach (id, e, order, s) -> []
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
			| Break -> [Jsr (-3)] (* Indicate it's a break *)
			| Continue -> [Jsr (-4)] (* Indicate it's a continue *)
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
		block StringMap.empty 0 0 assign_globals @
		[Jsr (StringMap.find "main" func_index); Hlt]
	in
	let text = 
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
			[Ent num_locals] @
			block local_index (num_locals + 1) num_params f.body @
			[Ret num_params]
		in
		let func_bodies = List.map trans_func program.functions in
		let (_, func_offsets) = (* generate the function offsets as a list *)
				List.fold_left (fun (i, offsets) body ->
						(* each step plus the number of byte statements *)
						((List.length body) + i, i::offsets))
					(* start from the end of global variable initialization *)
					((List.length data) + 1, []) func_bodies
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
				(i + 1)) 1 text
		);
		close_out fout