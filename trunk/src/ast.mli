(*Author : Akash, Yan Zou, Jiabin Hu, Shuai Sun *)
(*Most of the code is taken as it is form MicroC compiler and I will revise this file*)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Conn | Plus_asn |Mins_asn |Tims_asn |Div_asn |Mod_asn |Or |And |Not |Geq |Mod  
type uniop = Dollar | At | Deg_a | Dot | Hsh
type expr = (* Expressions *)
Literal of int (* 42 *)
| Id of string (* foo *)
| Binop of expr * op * expr (* a + b *)
| Assign of string * expr (* foo = 42 *)
| Call of string * expr list (* foo(1, 25 *)
| Noexpr (* While() *)
| Lbrk * expr * Rbrk
| uniop*expr
type stmt = (* Statements *)
Block of stmt list (* { ... } *)
| Expr of expr (* foo = bar + 3; *)
| Return of expr (* return 42; *)
| If of expr * stmt * stmt (* if (foo == 42) {} else {} *)
| Foreach of expr in expr by inorder stmt   (* for each loop *)
| Foreach of expr in expr by preorder stmt   (* for each loop *)
| Foreach of expr in expr by postorder stmt   (* for each loop *)
| Foreach of expr in expr by levelorder stmt   (* for each loop *)
| For of expr * expr * expr * stmt (* for loop *)
| Do of stmt * while * expr   (*do while loop *)
| While of expr * stmt (* while (i<10) { i = i + 1 } *)
type func_decl = {
typespecifier : string (* should be of type string ? *)
fname : string; (* Name of the function *)	
formals : string list; (* Formal argument names *)
locals : string list; (* Locally defined variables *)
body : stmt list;
}
type program = string list * func_decl list (* function return type function body and contents *)
