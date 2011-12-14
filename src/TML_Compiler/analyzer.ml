
(* analyzer.ml *)
(* @authors: Yan Zou *)
(* @description: Semantic Analysis and convert AST to SAST *)

open Type
open Ast
open Sast

module StringMap = Map.Make(String)

type symbol_table = {
	parent : symbol_table option;
	var : (t * string) list;
	func : (t * string * (t list)) list
}

let int_of_bool = function
	| true -> 1
	| false -> 0

let rec find_var scope name = 
	try
		List.find (fun (_, var_name) -> var_name = name) scope.var
	with Not_found -> 
		match scope.parent with
			| Some(parent) ->find_var parent name
			| _ -> raise Not_found

let rec find_func scope name =
	try
		List.find (fun (_, func_name, _) -> func_name = name) scope.func
	with Not_found -> 
		match scope.parent with
			| Some(parent) -> find_func parent name
			| _ -> raise Not_found

let part decl = 
		let t = fst decl in
		List.rev (List.fold_left (fun l -> function
			| WithInit (name, e) -> (t, name, Some(e))::l
			| WithoutInit name -> (t, name, None)::l) [] (snd decl))

let rec expr scope = function
	| Ast.Literal lit ->
			let lit_type = match lit with
				| IntLit i -> Int
				| FloatLit f -> Float
				| CharLit c -> Char
				| BoolLit b -> Boolean
				| StringLit s -> String
				| TreeLit -> Tree_type("~") (* universal tree type *)
			in Sast.Literal(lit), lit_type
  | Ast.Id id ->
			let (var_type, _) = try
					find_var scope id
				with Not_found ->
					raise (Failure ("undeclared identifier " ^ id))
			in Sast.Id(id), var_type
  | Ast.Binop (e1, bin_op, e2) ->
			let et1 = expr scope e1 and et2 = expr scope e2 in
			let (_, t1) = et1 and (_, t2) = et2 in
			if t1 = t2 then (* Type match *)
				Sast.Binop(et1, bin_op, et2), t1
			else
				raise (Failure ("type mismatch in binary operation"))
	| Ast.Assign (e1, e2) ->
			let et1 = expr scope e1 and et2 = expr scope e2 in
			let (_, t1) = et1 and (_, t2) = et2 in
			if t1 = t2 then (* Type match *)
				Sast.Assign(et1, et2), t1
			else
				raise (Failure ("type mismatch in assignment"))
  | Ast.Call (func_name, params) -> 
			let (func_type, _, required_param_types) = try
					find_func scope func_name
				with Not_found -> 
					raise (Failure ("undeclared identifier " ^ func_name))
			in
			let typed_params = List.map (expr scope) params in
			let param_types = List.map (fun et -> snd et) typed_params in
			if param_types = required_param_types then
				Sast.Call(func_name, typed_params), func_type
			else
				raise (Failure ("calling function " ^ func_name ^ " parameters mismatch"))
  | Ast.Uniop (un_op, e) ->
			let et = expr scope e in
			Sast.Uniop(un_op, et), (snd et)
  | Ast.Conn (e, el) -> 
			let et = expr scope e and etl = List.map (expr scope) el in
			Sast.Conn(et, etl), (snd et) (* TODO *)
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
				parent = Some(scope);
				var = [];
			} in
			(* check each statement in the block. scope may change in this process *)
			let check_stmt (block_scope, stl) s = match s with
				| Ast.Vardecl var_decl -> 
						let var_list = part var_decl in (* expand the multi-var declaration *)
						(* add all var in var_list to block_scope *)
						(* and convert the var_list(Ast.expr) to new_var_list(Sast.expr) *)
						let add_var (block_scope, new_var_list) (t, name, init) =
							let new_scope = { (* add var to block_scope, inverse order *)
							  (* We don't need init info in block_scope *)
								block_scope with var = (t, name)::block_scope.var
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
			Sast.Block(List.rev stl, List.rev block_scope.var)
	| Ast.Expr e -> Sast.Expr(expr scope e)
	| Ast.Return e -> Sast.Return(expr scope e)
	| Ast.ReturnVoid -> Sast.Return(Sast.Noexpr, Void)
	| Ast.If (e, s1, s2) ->
			let et = expr scope e in
			let st1 = stmt scope s1 and st2 = stmt scope s2 in
			Sast.If(et, st1, st2)
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
			Sast.For (expr scope e1, expr scope e2, expr scope e3, stmt scope s)
	| Ast.Do (s, e) ->
			Sast.Do (stmt scope s, expr scope e)
	| Ast.While (e, s) ->
			Sast.While (expr scope e, stmt scope s)
	| Ast.Break -> Sast.Break
	| Ast.Continue -> Sast.Continue
	| Ast.Empty -> Sast.Empty
	| Ast.Vardecl var_decl -> (* separately handled before *)
			raise (Failure "Variable declaration in wrong position!") 

let check program =
	let init_glob_scope = {
		parent = None;
		var = [];
		func = [];
	} in
	(* Global variable declarations and function definitions can be rearranged *)
	(* But local variable declarations can not be rearranged with other statements *)
	let (tree_list, glob_list, func_list) =
		(* expand multiple variable declarations into declaration lists *)
		let rec classify tree_list glob_list func_list glob_scope = function
			| [] -> (tree_list, glob_list, List.rev func_list)
			| (Globalvar var_decl)::tl -> (* put it into the loop *)
					let var_list = part var_decl in
					let new_var_list = (* convert Ast.expr to Sast.expr *)
						List.map (fun (t, name, init) ->
							(t, name, check_init glob_scope init)) var_list
					in
					let new_scope = { glob_scope with var =
						glob_scope.var @ (* add only type and name info into scope *)
						List.map (fun (vtype, name, init) -> (vtype, name)) var_list
					} in (* add new_var_list to glob_list in normal order *)
					classify tree_list (glob_list @ new_var_list) func_list new_scope tl
			| (Funcdef func_decl)::tl ->
					let ft = func_decl.Ast.return_type
						and fn = func_decl.Ast.fname
						and fp = func_decl.Ast.params
					in
					let required_param_types = 
						List.map (fun p -> fst p) fp
					in
					let new_scope = { glob_scope with func =
						(ft, fn, required_param_types)::glob_scope.func
					} in
					let func_block = 
						stmt glob_scope (Ast.Block(func_decl.Ast.body))
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
			| (Treedef tree_def)::tl ->
					classify tree_list glob_list func_list glob_scope tl
		in classify [] [] [] init_glob_scope (List.rev program)
	in {
		treetypes = tree_list;
		globals = glob_list;
		functions = func_list
	}