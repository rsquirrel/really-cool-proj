type op = Add | Sub | Mult | Div | Equal | Neq | Less_than | Leq | Greater_than | Or |And |Not |Geq |Mod |Dollar | At | Deg_a | Dot | Hsh | Child

type type_specifier = Int | Float | Char | String | Boolean | Void | Tree_type of string(* including return type of even main function *)

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
  | Uniop of op*expr   (*for unary operators *)
  | Conn of expr * (expr list)

type init = WithInit of string * expr
			| WithoutInit of string

type init_list = init list

type var_decl = type_specifier * init_list

type tree_def = {
  typename: string;
	members : var_decl list;
	degree :int;
	aliases : string list;
}
  
type traverse_order = Preorder | Inorder | Postorder | Levelorder (* different traversal orders *)

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
    params : (type_specifier * string) list;
    body : stmt list;
}

type construct = 
     Globalvar of var_decl
   | Funcdef of func_decl
   | Treedef of tree_def
               
type program = construct list 