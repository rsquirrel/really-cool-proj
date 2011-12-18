
(* analyzer.ml *)
(* @authors: Yan Zou *)
(* @description: Semantic Analysis and convert AST to SAST *)

open Type
open Sast

module StringMap = Map.Make(String)

type tree_table = {
	type_name : string;
	degree : int; (* convert all degrees to numbers in analyzer *)
	aliases : int StringMap.t; (* convert all aliases to number *)
	member_list : (t * string) list
}

type symbol_table = {
	parent : symbol_table option;
	vars : (t * string) list;
	funcs : (t * string * (t list)) list;
	trees : tree_table list;
	child_enum : int StringMap.t;
}

let int_of_bool = function
	| true -> 1
	| false -> 0

let string_of_type = function
	| Int -> "integer"
	| Float -> "float"
	| Char -> "character"
	| String -> "string"
	| Boolean -> "boolean"
	| Tree_type tname -> ("Tree(" ^ tname ^ ")")
	| Void -> "void"

let string_of_op = function
	  Add -> "+"
	| Sub -> "-"
	| Mult -> "*" 
	| Div -> "/"
	| Mod -> "%"
	| Equal -> "=="
	| Neq -> "!="
	| Less_than -> "<"
	| Leq -> "<="
	| Greater_than -> ">"
	| Geq -> ">="
	| Or -> "||"
	| And -> "&&"
	| Not -> "!"
	| Dollar -> "$"
	| At -> "@"
	| Father -> "^"
	| Deg_a -> "Deg"
	| Dot -> "."
	| Hsh -> "#"
	| Child -> "[]"

(* check if two types are same *)
(* ~ should be regarded as the same type with all types of trees *)
let is_same_type t1 t2 =
	match (t1, t2) with
		| (Tree_type tt1, Tree_type tt2) -> 
				if tt1 = "~" || tt2 = "~" then true
				else (tt1 = tt2)
		| (t1, t2) -> t1 = t2

let rec find_var scope name = 
	try
		List.find (fun (_, var_name) -> var_name = name) scope.vars
	with Not_found -> 
		match scope.parent with
			| Some(parent) ->find_var parent name
			| _ -> raise Not_found
(*
let rec find_func scope name =
	try
		List.find (fun (_, func_name, _) -> func_name = name) scope.func
	with Not_found -> (* for functions, we don't need to trace back to the root *)
		match scope.parent with
			| Some(parent) -> find_func parent name
			| _ -> raise Not_found *)

(* for trees, we don't need to trace back to the root *)
let find_tree scope name =
	try
		List.find (fun tree -> tree.type_name = name) scope.trees
	with Not_found ->
		raise (Failure ("Tree type " ^ name ^ " not declared"))

(* break the Ast.var_decl into lists of Sast.var_decl *)
let part decl = 
		let t = fst decl in
		List.rev (List.fold_left (fun l -> function
			| Ast.WithInit (name, e) -> (t, name, Some(e))::l
			| Ast.WithoutInit name -> (t, name, None)::l) [] (snd decl))

let e_op_type op t =
	raise (Failure ("The operand of operator " ^
		(string_of_op op) ^ " can not be " ^ (string_of_type t)))

let rec expr scope = function
	| Ast.Literal lit ->
			let lit_type = match lit with
				| IntLit i -> Int
				| FloatLit f -> Float
				| CharLit c -> Char
				| BoolLit b -> Boolean
				| StringLit s -> String
				| TreeLit -> Tree_type("~") (* universal tree type *)
			in
			Sast.Literal(lit), lit_type
  | Ast.Id id ->
			(try (* tree aliases first *)
				let i = StringMap.find id scope.child_enum in
					Sast.Literal(IntLit(i)), Int
			with Not_found ->
				try (* from local to global *)
					let (var_type, _) = find_var scope id in
						Sast.Id(id), var_type
				with Not_found ->
					raise (Failure ("undeclared identifier " ^ id)))
  | Ast.Binop (e1, bin_op, e2) ->
			let et1 = expr scope e1 in
			if bin_op = Dot then (* specially for tree operators *)
				match (snd et1) with
					| Tree_type tname ->
							let tree = find_tree scope tname in
							(* the expr following dot can only be ID of tree members *)
							let tree_scope = { scope with
								parent = None; vars = tree.member_list }
							in
							let et2 = expr tree_scope e2 in
							Sast.Binop(et1, bin_op, et2), (snd et2)
					| _ ->
						raise (Failure ("The left operand of operator . should be a tree type"))
			else if bin_op = Child then (* specially for tree operators *)
				match (snd et1) with
					| Tree_type tname -> 
							let tree = find_tree scope tname in
							let tree_scope = { scope with child_enum = tree.aliases } in
							let et2 = expr tree_scope e2 in
							if (snd et2) = Int then
								Sast.Binop(et1, bin_op, et2), Tree_type(tname)
							else
								raise (Failure ("The expression inside [] should be an integer"))
							(*let et2 =
								try match e2 with
									| Ast.Id id -> (* if it is an ID, search it in alias names *)
											let i = StringMap.find id tree.aliases in
											Sast.Literal(InitLit(i)), Int
									| _ -> raise Not_found
								with Not_found -> (* not in alias names, evaluate it as usual *)
									let et2 = expr scope e2 in
									if (snd et2) = Int then et2
									else
										raise (Failure ("The expression inside [] should be an integer"))
							in Sast.Binop(et1, bin_op, et2), Tree_type(tname) *)
					| _ ->
						raise (Failure ("The left operand of operator [] should be a tree type"))
			else
				let et2 = expr scope e2 in
				let (_, t1) = et1 and (_, t2) = et2 in
				 (*
				let e_right error =
					raise (Failure ("Right operand of operator " ^
						(string_of_op bin_op) ^ " can not be " ^ (string_of_type t2)))
				in 
				let check_match = match bin_op with
					| Add | Sub | Mult | Div | Mod
					| Equal | Neq | Less_than | Leq | Greater_than | Geq -> 
				in*)
				let t = (* operands should have the same type except tree operators *)
					if not (is_same_type t1 t2) then
							raise (Failure ("Type mismatch for operator " ^
								(string_of_op bin_op) ^ ": left - " ^ (string_of_type t1) ^
																				", right - " ^ (string_of_type t2)))
					else match bin_op with (* check operand type for different operators *)
						| Add -> (match t1 with
									| Int | Float | String -> t1
									| _ -> e_op_type bin_op t1)
						| Sub | Mult | Div -> (match t1 with
									| Int | Float -> t1
									| _ -> e_op_type bin_op t1)
						| Mod -> (match t1 with
									| Int -> t1
									| _ -> e_op_type bin_op t1)
						| Equal | Neq -> (match t1 with
									| Int | Float | Char | String | Boolean
									| Tree_type _ -> Boolean
									| _ -> e_op_type bin_op t1)
						| Less_than | Leq | Greater_than | Geq -> (match t1 with
									| Int | Float | Char | String -> Boolean
									| _ -> e_op_type bin_op t1)
					  | And | Or -> (if t1 = Boolean then Boolean else
							raise (Failure ("Only boolean is allowed for boolean operators")))
						| _ -> t1 (* TODO Tree operators *)
				in
				Sast.Binop(et1, bin_op, et2), t
	| Ast.Assign (e1, e2) ->
			let et1 = expr scope e1 and et2 = expr scope e2 in
			let (_, t1) = et1 and (_, t2) = et2 in
			if is_same_type t1 t2 then (* Type match *)
				Sast.Assign(et1, et2), t1
			else
				raise (Failure ("type mismatch in assignment"))
  | Ast.Call (func_name, params) -> 
			let (func_type, _, required_param_types) = try
					List.find (fun (_, fname, _) -> fname = func_name) scope.funcs
				with Not_found -> 
					raise (Failure ("undeclared identifier " ^ func_name))
			in
			let typed_params = List.map (expr scope) params in
			let param_types = List.map (fun et -> snd et) typed_params in
			if func_name = "print" or
					func_name = "alloc" or
					param_types = required_param_types then
				Sast.Call(func_name, typed_params), func_type
			else
				raise (Failure ("calling function " ^ func_name ^ " parameters mismatch"))
  | Ast.Uniop (un_op, e) ->
			let et = expr scope e in
			let t = snd et in
		  if un_op = Deg_a then
				match t with
					| Tree_type tname ->
							let tree = find_tree scope tname in
							Sast.Literal(IntLit tree.degree), Int
					| _ ->
						raise (Failure ("Operator & takes only tree-typed operand"))
			else
				let tt = match un_op with
					| Add -> if t = Int or t = Float then t else
							raise (Failure ("Only integers and floats can be added a positive sign"))
					| Sub -> if t = Int or t = Float then t else
							raise (Failure ("Only integers and floats can be added a negative sign"))
					| Not -> if t = Boolean then Boolean else
							raise (Failure ("Only boolean is allowed for boolean operators"))
					| Dollar | At | Father | Hsh ->
							(match t with (* the existence of tree has been checked when declared *)
								| Tree_type tname -> if un_op = Hsh then Int else t
								| _ ->
									raise (Failure ("Operator " ^ string_of_op un_op ^
										" takes only tree-typed operand")))
					| _ -> raise (Failure ("The operator " ^
									(string_of_op un_op) ^ " is not unary"))
				in
				Sast.Uniop(un_op, et), tt
  | Ast.Conn (e, el) -> 
			let et = expr scope e and etl = List.map (expr scope) el in
			let t1 = snd et in
			(match t1 with
				| Tree_type tname ->
						let tree = find_tree scope tname in
						if (List.length etl) > tree.degree then
							raise (Failure ("Too many children in tree connection "^
								"for tree-type " ^ tname ^ " whose degree is only " ^
								(string_of_int tree.degree)))
						else
							let _ = List.iter (fun (_, t2) ->
								if (t2 <> Tree_type("~") && t2 <> t1) then
									raise (Failure ("Tree type mismatch in tree connection"))
							) etl
							in
							Sast.Conn(et, etl), t1
				| _ ->
					raise (Failure ("Operator & takes only tree-typed operand")))
	| Ast.Noexpr ->
			Sast.Noexpr, Void (* TODO: What is this? *)

(* convert Ast.expr to Sast.expr in the variable initialization *)
let check_init scope = function
	| Some(e) -> Some(expr scope e)
	| None -> None

let rec stmt scope = function
	| Ast.Block sl ->
			(* create a new empty scope for the block *)
			let block_scope = { scope with (* func reserves *)
				parent = Some(scope); vars = []; }
			in
			(* check each statement in the block. scope may change in this process *)
			let check_stmt (block_scope, stl) s = match s with
				| Ast.Vardecl var_decl ->
						let _ = match (fst var_decl) with (* check tree type *)
							| Tree_type tname -> ignore(find_tree block_scope tname)
							| _ -> ()
						in
						let var_list = part var_decl in (* expand the multi-var declaration *)
						(* add all var in var_list to block_scope *)
						(* and convert the var_list(Ast.expr) to new_var_list(Sast.expr) *)
						let add_var (block_scope, new_var_list) (t, name, init) =
							let new_scope = { (* add var to block_scope, inverse order *)
							  (* We don't need init info in block_scope *)
								block_scope with vars = (t, name)::block_scope.vars
							} and new_var_decl = (* convert from Ast.expr to Sast.expr *)
								(t, name, check_init scope init)
							in (* add new_var_decl to new_var_list, inverse order *)
							(new_scope, Sast.Vardecl(new_var_decl)::new_var_list)
						in
						let (new_scope, new_var_list) =
							List.fold_left add_var (block_scope, []) var_list
						in
						(new_scope, new_var_list @ stl) (* for one declaration *)
				| _ -> (block_scope, (stmt block_scope s)::stl)
			in
			let (block_scope, stl) =
				List.fold_left check_stmt (block_scope, []) sl
			in (* Note we don't have init info in block_scope *)
			(* The init is still in stmt list which can not be rearranged *)
			Sast.Block(List.rev stl, List.rev block_scope.vars)
	| Ast.Expr e -> Sast.Expr(expr scope e)
	| Ast.Return e -> Sast.Return(expr scope e)
	| Ast.ReturnVoid -> Sast.Return(Sast.Noexpr, Void)
	| Ast.If (e, s1, s2) ->
			let et = expr scope e in
			if ((snd et) = Boolean) then
				let st1 = stmt scope s1 and st2 = stmt scope s2 in
				Sast.If(et, st1, st2)
			else
				raise (Failure ("the expression in if statement is not boolean"))
	| Ast.Foreach (id, e, order, s) ->
			let (id_type, _) = try
					find_var scope id
				with Not_found ->
					raise (Failure ("undeclared identifier " ^ id))
			in (match id_type with
				| Tree_type type_name ->
						let et = expr scope e in 
						if (snd et) = id_type then (* The same tree type *)
							Sast.Foreach(id, et, order, stmt scope s)
						else
							raise (Failure ("foreach not in same tree type"))
				| _ -> raise (Failure ("identifier " ^ id ^ " is not a tree type")))
	| Ast.For (e1, e2, e3, s) -> 
			let et2 = expr scope e2 in
			if ((snd et2) = Boolean) then
				Sast.For (expr scope e1, et2, expr scope e3, stmt scope s)
			else
				raise (Failure ("the second expression in for statement is not boolean"))
	| Ast.Do (s, e) ->
			let et = expr scope e in
			if ((snd et) = Boolean) then
				Sast.Do (stmt scope s, expr scope e)
			else
				raise (Failure ("the expression in do statement is not boolean"))
	| Ast.While (e, s) ->
			let et = expr scope e in
			if ((snd et) = Boolean) then
				Sast.While (expr scope e, stmt scope s)
			else
				raise (Failure ("the expression in while statement is not boolean"))
	| Ast.Break -> Sast.Break
	| Ast.Continue -> Sast.Continue
	| Ast.Empty -> Sast.Empty
	| Ast.Vardecl var_decl -> (* separately handled before *)
			raise (Failure "Variable declaration in wrong position!") 

let check program =
	let init_glob_scope = {
		parent = None;
		vars = [];
		funcs = [(Tree_type("~"), "alloc", []); (Void, "print", [])]; (* built-in functions *)
		trees = [];
		child_enum = StringMap.empty;
	} in
	(* Global variable declarations and function definitions can be rearranged *)
	(* But local variable declarations can not be rearranged with other statements *)
	let (tree_list, glob_list, func_list) =
		(* expand multiple variable declarations into declaration lists *)
		let rec classify tree_list glob_list func_list glob_scope = function
			| [] -> (List.rev tree_list, glob_list, List.rev func_list)
			| (Ast.Globalvar var_decl)::tl -> (* put it into the loop *)
					let _ = match (fst var_decl) with (* check tree type *)
						| Tree_type tname -> ignore(find_tree glob_scope tname)
						| _ -> ()
					in
					let var_list = part var_decl in
					let new_var_list = (* convert Ast.expr to Sast.expr *)
						List.map (fun (t, name, init) ->
							(t, name, check_init glob_scope init)) var_list
					in
					let new_scope = { glob_scope with vars =
						glob_scope.vars @ (* add only type and name info into scope *)
						List.map (fun (vtype, name, _) -> (vtype, name)) var_list
					} in (* add new_var_list to glob_list in normal order *)
					classify tree_list (glob_list @ new_var_list) func_list new_scope tl
			| (Ast.Funcdef func_decl)::tl ->
					let ft = func_decl.Ast.return_type
						and fn = func_decl.Ast.fname
						and fp = func_decl.Ast.params
					in
					let required_param_types = 
						List.map (fun p -> fst p) fp
					in
					let new_scope = { glob_scope with funcs =
						(ft, fn, required_param_types)::glob_scope.funcs
					} in
					let param_scope = { new_scope with 
						parent = Some(new_scope); vars = fp}
					in
					let func_block = 
						stmt param_scope (Ast.Block(func_decl.Ast.body))
					in
					let (new_body, local_var) = match func_block with
						| Sast.Block(new_body, local_var) -> (new_body, local_var)
						| _ -> raise (Failure ("unexpected fatal error"))
					in
					let new_func_decl = {
						return_type = ft;
						fname = fn;
						params = fp;
						locals = local_var;
						(* Note we don't have init info in local_var *)
						(* The init is still in function body which can not be rearranged *)
						body = new_body;
					} in (* add new_func_decl to func_list in reverse order *)
					classify tree_list glob_list (new_func_decl::func_list) new_scope tl
			| (Ast.Treedef tree_def)::tl ->
					let tn = tree_def.Ast.typename in
					let (alias_map, _) =
						List.fold_left (fun (m, i) s ->
							(StringMap.add s i m, i + 1))
							(StringMap.empty, 0) (tree_def.Ast.aliases)
					in
					let tm =
						List.concat (List.map part tree_def.Ast.members)
					in
					let tree_table = {
						type_name = tn;
						degree = tree_def.Ast.degree;
						aliases = alias_map;
						member_list = List.map (fun (t, name, _) -> (t, name)) tm
					} in
					let new_scope = { glob_scope with trees = 
						tree_table::glob_scope.trees }
					in
					let new_tm = (* convert Ast.expr to Sast.expr *)
						List.map (fun (t, name, init) ->
							let _ = match t with (* recursive tree type in members *)
								| Tree_type tname -> ignore(find_tree new_scope tname)
								| _ -> ()
							in (t, name, check_init glob_scope init)) tm
					in
					let new_tree_def = {
						typename = tn;
						members = new_tm;
						Sast.degree = tree_def.Ast.degree;
					} in
					classify (new_tree_def::tree_list) glob_list func_list new_scope tl
		in classify [] [] [] init_glob_scope (List.rev program)
	in {
		treetypes = tree_list;
		globals = glob_list;
		functions = func_list
	}