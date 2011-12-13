(*let lexbuf = Lexing.from_channel stdin in
	Parser.program Scanner.token lexbuf*)
	
(* compiler.ml *)
(* @authors: Yan Zou *)

open Type
open Sast
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
	List.iter (fun (_, name, _) -> print_endline name) program.globals