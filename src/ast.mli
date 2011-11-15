(*Author : Akash, Yan Zou, Jiabin Hu, Shuai Sun *)
(*Most of the code is taken as it is form MicroC compiler and I will revise this file*)

type op = Add | Sub | Mult | Div | Equal | Neq | Less_than | Leq | Greater_than | Conn |Or |And |Not |Geq |Mod (*| these operators are removed: Plus_asn |Mins_asn |Tims_asn |Div_asn |Mod_asn*)  

type uniop = Dollar | At | Deg_a | Dot | Hsh

type type_specifier = Int | Float | Char | String | Boolean | Void | Tree_type(* including return type of even main function *)

type type_def = Tree_type * Less_than * int * Greater_than * string             (* treetype < 2 > MyTree_t *)
                | Tree_type * Less_than * int * string * Greater_than * string   (* treetype <2,[string,string.....]> MyTree_t*)

type expr = (* Expressions *)
	Literal of int (* 42 *)
  | type_def of string (* MyTree_t bfs*)
  | Id of string (* foo *)
  | Binop of expr * op * expr (* a + b *)
  | Assign of string * expr (* foo = 42 *)
  | Call of string * expr list (* foo(1, 25) *)
  | Noexpr (* While() *)
  | Lbrk * expr * Rbrk (* parentisized expressions *)
  | uniop * expr  (* for unary operators *)
  
type traverse_order = Preorder | Inorder | Postorder | Levelorder (* different traversal orders *)
 
type stmt = (* Statements *)
   |                    (* nothing *)
   | Block of stmt list (* { ... } *)
   | Expr of expr (* foo = bar + 3; *)
   | Return of expr (* return 42; *)
   | If of expr * stmt * stmt (* if (foo == 42) {} else {} *)
   | Foreach of expr in expr  by traverse_order stmt   (* for each loop *)
   | For of expr * expr * expr * stmt (* for loop *)
   | Do of stmt * while * expr   (*do while loop *)
   | While of expr * stmt (* while (i<10) { i = i + 1 } *)
   | Break of expr (* break *)
   | Continue of expr (* continue *)
   

type param_decl = type_specifier * string

type param_list = param_decl 
                | param_decl * param_list

type func_def = type_specifier * string * param_list 

type program = func_def * stmt
