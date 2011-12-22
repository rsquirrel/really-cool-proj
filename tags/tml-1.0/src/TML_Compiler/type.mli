type t = 
	| Int 
	| Float 
	| Char 
	| String 
	| Boolean 
	| Void 
	| Tree_type of string(* including return type of even main function *)

type literal = 
    IntLit of int
  | FloatLit of float
  | CharLit of char
  | BoolLit of bool
  | StringLit of string
  | TreeLit (* can only be ~ *)

type op =
	| Add 
	| Sub 
	| Mult 
	| Div 
	| Mod 
	| Equal 
	| Neq 
	| Less_than 
	| Leq 
	| Greater_than 
	| Geq 
	| Or 
	| And 
	| Not 
	| Dollar 
	| At 
	| Deg_a 
	| Hsh 
	| Dot 
	| Child
	| Father

type traverse_order = 
	| Preorder 
	| Inorder 
	| Postorder 
	| Levelorder (* different traversal orders *)
