(* bytecode.mli *)
(* @authors: Yan Zou *)

type bstmt =
	| Psh of int (* Push a literal *)
	| Pop (* Pop out a literal from the stack *)
(*	| Pop of int (* Pop out several elements from the stack *)*)
	| Uop of Type.op (* Perform unary operation on top one element of stack *)
	| Bin of Type.op (* Perform binary operation on top two elements of stack *)
	| Lod of int (* Fetch global variable *)
	| Str of int (* Store global variable *)
	| Lfp of int (* Load frame pointer relative *)
	| Sfp of int (* Store frame pointer relative *)
	| Jsr of int (* Call function by absolute address *) 
	| Ent of int (* Entry of a function *)
	| Ret of int (* Restore FP, SP, consume formals, push result *) 
	| Beq of int (* Branch relative if top-of-stack is zero *)
	| Bne of int (* Branch relative if top-of-stack is non-zero *) 
	| Bra of int (* Branch relative *)
	| Hlt (* Terminate *) 
	| Fld of int (* Add a new value field to a tree node *)
	| Glb of int (* Indicate the number of global variables *)
