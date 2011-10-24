(* scanner.mll *)
(* @authors: Shuai Sun *)

{ open Parser }

(* letter, digit *)
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let character = ['\b' '\t' '\n' '\r' '\\' '_' ' ' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '{' '}' '|' ';' '<' '>' '.' ',' '?' '/' '+' '-' '=' '~']
			| letter | digit

rule token = parse
	| "/*"                 { comment lexbuf }
	| "//"                 { line_comment lexbuf }
	| [' ' '\t' '\r' '\n'] { token lexbuf }
	| letter (letter | digit | '_')* as identifier 
                           { ID(identifier) }
	(* Keywords *)
	| "if"            { IF }
	| "else"          { ELSE }
	| "while"         { WHILE }
	| "foreach"       { FOREACH }
	| "in"            { IN }
	| "by"            { BY }
	| "preorder"      { PREORDER }
	| "inorder"       { INORDER }
	| "postoder"      { POSTORDER }
	| "levelorder"    { LEVELORDER }
	| "return"        { RETURN }
	| "int"           { INT_T }
	| "float"         { FLOAT_T }
	| "char"          { CHAR_T }
	| "string"        { STRING_T }
	| "bool"          { BOOL_T }
	| "void"          { VOID }
	| "treetype"      { TREETYPE }

	(* Constants *)
	| digit+          as integer        
                      { INT(int_of_string interger) }
	| digit+ '.' digit* 
	| '.' digit+ ('e' ['+''-']? digit+)?
	| digit+ ('.' digit+)? 'e' digit+      as float
                      { FLOAT(float_of_string float) }
	| "true"
	| "false"         as bool
                      { BOOL(bool_of_string bool) }
	| '"' (character | '\'')+ '"'   as string
                      { STRING(string) }
	| '\'' (character | '"') '\''   as char
                      { CHAR(char) }
	| '~'             { NULL }


	| '{'    { LBRACE }
	| '}'    { RBRACE }
	| ';'    { SEMI }

	| '='    { ASSIGN }
	
	(* Operators *)
	| "->"   { CONNECT }
	| "+="   { PLUS_ASN }
	| "-="   { MINUS_ASN }
	| "*="   { TIMES_ASN }
	| "/="   { DIVIDE_ASN }
	| "%="   { MOD_ASN }
	| "||"   { OR }
	| "&&"   { AND }
	| '!'    { NOT }
	| "!="   { NEQ }
	| '>'    { GT }
	| '<'    { LT }
	| "<="   { LEQ }
	| ">="   { GEQ }
	| "=="   { EQ }
	| '+'    { PLUS }
	| '-'    { MINUS }
	| '*'    { TIMES }
	| '/'    { DIVIDE }
	| '%'    { MOD }
	| '$'    { DOLLAR }
	| '@'    { AT }
	| '['    { LBRACK }
	| ']'    { RBRACK }
	| '&'    { DEG_AND }
	| '.'    { DOT }
	| '#'    { HASH }
	| '('    { LPAREN }
	| ')'    { RPAREN }

	| eof { EOF }
	| _ as err_char { raise (Failure("illegal character " ^ Char.escaped err_char)) }

(* comment *)
and line_comment = parse
	'\n'     { token lexbuf }
	| _      { line_comment lexbuf }

and comment = parse
	"*/"     { token lexbuf }
	| _      { comment lexbuf }
