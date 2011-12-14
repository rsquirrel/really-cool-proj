(* sast.mli *)
(* @authors: Yan Zou *)

open Type

type expr_c = (* Expressions *)
    Literal of literal (* 42 *)
  | Id of string (* foo *)
  | Binop of expr * op * expr (* a + b *)
  | Assign of expr * expr (* a = b *)
  | Call of string * (expr list) (* foo(1, 25) *)
  | Noexpr (* While() *)
  | Uniop of op * expr  (*for unary operators *)
  | Conn of expr * (expr list)
and expr = expr_c * t

type var_decl = t * string * (expr option)

type tree_def = {
  typename: string;
	members : var_decl list;
	degree : int;
	aliases : string list;
}

type stmt = (* Statements  nothing *)
     Block of (stmt list) * ((t * string) list) (* statement list and var list *)
   | Expr of expr   (*foo = bar + 3; *)
   | Return of expr (* return 42 also includes return function_name *)
   | If of expr * stmt * stmt (* if (foo == 42) {} else {} *)
   | Foreach of string * expr * traverse_order * stmt   (* for each loop *)
   | For of expr * expr * expr * stmt (* for loop *)
   | Do of stmt * expr   (*do while loop *)
   | While of expr * stmt (* while (i<10) { i = i + 1 } *)
   | Break  (* break *)
   | Continue  (* continue *)
   | Vardecl of var_decl
   | Empty 
   
type func_decl = {
		return_type : t;
    fname : string;
    params : (t * string) list;
		locals : (t * string) list; (* we don't put init here, it's in body *)
    body : stmt list; (* variable init is a stmt that can not be rearranged *)
}
               
type program = {
		treetypes: tree_def list;
		globals: var_decl list;
		functions: func_decl list;
}