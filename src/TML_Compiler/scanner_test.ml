(* scanner_test.ml *)
(* @author: Yan Zou *)
(* To test the scanner by outputting all the recognized lexical words *)

open Parser;;

let lexbuf = Lexing.from_channel stdin in
let wordlist =
let rec next l =
	let word = 
		match Scanner.token lexbuf with
		  | EOF -> "EOF"
		  | ID(id) -> "ID(" ^ id ^ ")"
		  | IF -> "IF"
		  | ELSE -> "ELSE"
		  | WHILE -> "WHILE"
		  | DO -> "DO"
		  | FOR -> "FOR"
		  | BREAK -> "BREAK"
		  | CONTINUE -> "CONTINUE"
		  | FOREACH -> "FOREACH"
		  | IN -> "IN"
		  | BY -> "BY"
		  | RETURN -> "RETURN"
		  | PREORDER -> "PREORDER"
		  | INORDER -> "INORDER"
		  | POSTORDER -> "POSTORDER"
		  | LEVELORDER -> "LEVELORDER"
		  | INT_T -> "INT_T"
		  | FLOAT_T -> "FLOAT_T"
		  | CHAR_T -> "CHAR_T"
		  | STRING_T -> "STRING_T"
		  | BOOL_T -> "BOOL_T"
		  | VOID -> "VOID"
		  | TREETYPE -> "TREETYPE"
		  | INT(i) -> "INT(" ^ (string_of_int i) ^ ")"
		  | FLOAT(f) -> "FLOAT(" ^ (string_of_float f) ^ ")"
		  | BOOL(b) -> "BOOL(" ^ (string_of_bool b) ^ ")"
		  | STRING(s) -> "STRING(" ^ s ^ ")"
		  | CHAR(c) -> "CHAR(" ^ (String.make 1 c) ^ ")"
		  | NULL -> "NULL"
		  | LBRACE -> "LBRACE"
		  | RBRACE -> "RBRACE"
		  | SEMI -> "SEMI"
		  | COLON -> "COLON"
		  | COMMA -> "COMMA"
		  | ASSIGN -> "ASSIGN"
		  | CONNECT -> "CONNECT"
		  (* These operators are removed.
          | PLUS_ASN -> "PLUS_ASN"
		  | MINUS_ASN -> "MINUS_ASN"
		  | TIMES_ASN -> "TIMES_ASN"
		  | DIVIDE_ASN -> "DIVIDE_ASN"
		  | MOD_ASN -> "MOD_ASN"
          *)
		  | OR -> "OR"
		  | AND -> "AND"
		  | NOT -> "NOT"
		  | NEQ -> "NEQ"
		  | GT -> "GT"
		  | LT -> "LT"
		  | LEQ -> "LEQ"
		  | GEQ -> "GEQ"
		  | EQ -> "EQ"
		  | PLUS -> "PLUS"
		  | MINUS -> "MINUS"
		  | TIMES -> "TIMES"
		  | DIVIDE -> "DIVIDE"
		  | MOD -> "MOD"
		  | DOLLAR -> "DOLLAR"
		  | AT -> "AT"
		  | LBRACK -> "LBRACK"
		  | RBRACK -> "RBRACK"
		  | DEG_AND -> "DEG_AND"
		  | DOT -> "DOT"
		  | HASH -> "HASH"
		  | FATHER -> "FATHER"
		  | LPAREN -> "LPAREN"
		  | RPAREN -> "RPAREN"
		  (*| _ -> "UNKNOWN"*)
	  in if word = "EOF" then l
				else next (word :: l)
in next []
in
List.iter print_endline (List.rev wordlist)