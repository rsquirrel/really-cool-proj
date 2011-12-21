(* bytecode.mli *)
(* @authors: Yan Zou *)

type bstmt =
	| Glb of int (* Indicate the number of global variables *)
	| Psi of int (* Push an integer *)
	| Psf of float (* Push a floating number *)
	| Psc of char (* Push a cbaracter *)
	| Pss of string (* Push a string *)
	| Psb of bool (* Push a boolean *)
	| Pst (* Push a tree *)
(*	| Pop (* Pop out a literal from the stack *)*)
	| Pop of int (* Pop out several elements from the stack *)
	| Uop of (Type.op * Type.t) (* Perform unary operation on top one element of stack *)
	| Bin of (Type.op * Type.t) (* Perform binary operation on top two elements of stack *)
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
	| Alc of int (* new a tree for tree-typed variables, int is the degree *)
	| Fld of Type.t (* Add a new value field to a tree node *)
	| Sfd of int (* assign the value to the corresponding field of the tree *)
	| Scd (* assign the corresponding child of the tree, need three arguments *)
	| Nxt of int (* next iteration of foreach loop *)
	| Hlt (* Terminate *) 