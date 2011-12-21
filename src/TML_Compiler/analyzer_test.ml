(* analyzer_test.ml *)
(* @authors: Yan Zou *)

open Type
open Sast

let print_type = function
	| Int -> print_string "<int> "
	| Float -> print_string "<float> "
	| Char -> print_string "<char> "
	| String -> print_string "<string> "
	| Boolean -> print_string "<bool> "
	| Void -> print_string "<void> "
	| Tree_type tname -> print_string ("<tree:" ^ tname ^ "> ")

let print_order = function
	| Preorder -> print_string "preorder "
	| Inorder -> print_string "inorder "
	| Postorder -> print_string "postorder "
	| Levelorder -> print_string "levelorder "

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

let rec print_expr = function
	| Literal lit, t ->
			(match lit with
				| IntLit i -> print_int i
				| FloatLit f -> print_float f
				| CharLit c -> print_char c
				| BoolLit b -> print_string (string_of_bool b)
				| StringLit s -> print_string s
				| TreeLit -> print_char '~');
			print_type t
  | Id id, t ->
			print_string id; print_type t
  | Binop (e1, bin_op, e2), t ->
			print_expr e1; print_string (string_of_op bin_op);
			print_type t; print_expr e2
	| Assign (e1, e2), t ->
			print_expr e1; print_char '='; print_type t; print_expr e2
  | Call (func_name, params), t -> 
			print_string func_name;
			print_type t;
			print_char '(';
			List.iter print_expr params;
			print_string ") ";
  | Uniop (un_op, e), t ->
			print_string (string_of_op un_op); print_type t; print_expr e;
  | Conn (e, el), t -> 
			print_expr e; print_string "->"; print_type t; print_char '(';
			List.iter print_expr el; print_string ") "
	| Noexpr, t ->
			print_type t

let rec print_tab num =
	if (num > 0) then
		(print_char '\t'; print_tab (num - 1))
	else ()

let print_decl (t, name, init) =
	print_string ("declare: " ^ name); print_type t;
	match init with
		| None -> print_endline ";"
		| Some(e) ->
				print_string "= "; print_expr e; print_endline ";"

let print_local indent (t, name) = 
	print_tab indent;
	print_string ("local: " ^ name);
	print_type t;
	print_endline ";"

let rec print_stmt indent s =
	print_tab indent; match s with
		| Block (sl, vl) ->
				print_endline "{";
				List.iter (print_local (indent + 1)) vl;
				List.iter (print_stmt (indent + 1)) sl;
				print_tab indent; print_endline "}"
		| Expr e -> print_expr e; print_endline ";"
		| Return e -> print_string "return "; print_expr e; print_endline ";"
		| If (e, s1, s2) ->
				print_string "if ("; print_expr e; print_endline ")";
				let new_indent = match s1 with
					| Block (_, _) -> indent
					| _ -> indent + 1
				in
				print_stmt new_indent s1;
				print_tab indent;
				print_endline "else";
				let new_indent = match s2 with
					| Block (_, _) -> indent
					| _ -> indent + 1
				in
				print_stmt new_indent s2
		| Foreach (id, e, order, s) ->
				print_string "foreach ("; print_string id; print_expr e;
				print_order order; print_endline ")";
				let new_indent = match s with
					| Block (_, _) -> indent
					| _ -> indent + 1
				in
				print_stmt new_indent s
		| For (e1, e2, e3, s) -> 
				print_string "for (";
				print_expr e1; print_string "; ";
				print_expr e2; print_string "; ";
				print_expr e3; print_string "; ";
				print_endline ")";
				let new_indent = match s with
					| Block (_, _) -> indent
					| _ -> indent + 1
				in
				print_stmt new_indent s
		| Do (s, e) ->
				print_endline "do";
				let new_indent = match s with
					| Block (_, _) -> indent
					| _ -> indent + 1
				in
				print_stmt new_indent s;
				print_tab indent;
				print_string "while ("; print_expr e; print_endline ");";
		| While (e, s) ->
				print_string "while ("; print_expr e; print_endline ")";
				let new_indent = match s with
					| Block (_, _) -> indent
					| _ -> indent + 1
				in
				print_stmt new_indent s
		| Break i -> print_endline ("break " ^ (string_of_int i) ^ ";")
		| Continue i -> print_endline ("continue " ^ (string_of_int i) ^ ";")
		| Empty ->  print_endline ";"
		| Vardecl vd ->
				print_decl vd;;

let lexbuf = Lexing.from_channel stdin in
let program = Analyzer.check (Parser.program Scanner.token lexbuf) in
	List.iter print_decl program.globals;
	List.iter (fun decl ->
		print_string ("function: " ^ decl.fname);
		print_type decl.return_type;
		print_char '(';
		List.iter (fun (t, name) -> print_string name; print_type t) decl.params;
		print_endline ") ";
		print_stmt 0 (Block(decl.body, decl.locals))
	) program.functions;