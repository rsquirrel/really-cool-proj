
type op = Add | Sub | Mult | Div | Equal | Neq | Less_than | Leq | Greater_than | Conn |Or |And |Not |Geq |Mod |Dollar | At | Deg_a | Dot | Hsh  

type type_specifier = Int | Float | Char | String | Boolean | Void | Tree_type(* including return type of even main function *)


type expr = (* Expressions *)
  | Literal of int (* 42 *)
  | Type_def of string (* MyTree_t bfs*)
  | Id of string (* foo *)
  | Binop of expr * op * expr (* a + b *)
  | Assign of string * expr (* foo = 42 *)
  | Call of string * expr list (* foo(1, 25) *)
  | Noexpr (* While() *)
  | LbrkRbrk of expr (* parentisized expressions *)
  | Uniop of op*expr   (*for unary operators *)
  
type traverse_order = Preorder | Inorder | Postorder | Levelorder (* different traversal orders *)

type stmt = (* Statements  nothing *)
   | Expr of expr   (*foo = bar + 3; *)
   | Return of expr (* return 42 also includes return function_name *)
   | If of expr * stmt * stmt (* if (foo == 42) {} else {} *)
   | Foreach of expr * expr * traverse_order * stmt   (* for each loop *)
   | For of expr * expr * expr * stmt (* for loop *)
   | Do of stmt * expr   (*do while loop *)
   | While of expr * stmt (* while (i<10) { i = i + 1 } *)
   | Break of expr (* break *)
   | Continue of expr (* continue *)
   
type stmt_list = Seq of stmt * stmt_list | Single_stmt of stmt

type stmt_block = Block of stmt_list | Empty_block
   
type param_decl = type_specifier * string

type param_list = Seq of param_decl * param_list 
				| Single_decl of param_decl 
               
type program = type_specifier * string * param_list * stmt_block