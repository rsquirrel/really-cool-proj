(* sast.mli *)
(* @authors: Yan Zou *)

open Type

type literal = 
    IntLit of int
  | FloatLit of float
  | CharLit of char
  | BoolLit of bool
  | StringLit of string
  | TreeLit (* can only be ~ *)

type expr = (* Expressions *)
    Literal of literal (* 42 *)
  | Id of string (* foo *)
  | Binop of expr * op * expr (* a + b *)
  | Assign of expr * expr (* a = b *)
  | Call of string * (expr list) (* foo(1, 25) *)
  | Noexpr (* While() *)
  | Uniop of op * expr   (*for unary operators *)
  | Conn of expr * (expr list)

type var_decl = t * string * (Ast.expr option)

type tree_def = {
  typename: string;
	members : var_decl list;
	degree : int;
	aliases : string list;
}
  

type stmt = (* Statements  nothing *)
     Block of (stmt list)
   | Expr of expr   (*foo = bar + 3; *)
   | Return of expr (* return 42 also includes return function_name *)
   | ReturnVoid
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
    fname : string;
    params : (t * string) list;
		locals : var_decl list;
    body : stmt list;
}
               
type program = {
		treetypes: tree_def list;
		globals: var_decl list;
		functions: func_decl list;
}