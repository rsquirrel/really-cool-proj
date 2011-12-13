type t = 
	| Int 
	| Float 
	| Char 
	| String 
	| Boolean 
	| Void 
	| Tree_type of string(* including return type of even main function *)
	
type op =
	| Add 
	| Sub 
	| Mult 
	| Div 
	| Equal 
	| Neq 
	| Less_than 
	| Leq 
	| Greater_than 
	| Or 
	| And 
	| Not 
	| Geq 
	| Mod 
	| Dollar 
	| At 
	| Deg_a 
	| Dot 
	| Hsh 
	| Child

type traverse_order = 
	| Preorder 
	| Inorder 
	| Postorder 
	| Levelorder (* different traversal orders *)

