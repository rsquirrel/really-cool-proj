(* scanner.mll *)
(* @authors: Shuai Sun *)
(* modified by Jiabin Hu and Yan Zou on identifier and keyword order issues *)

{ 
open Parser 

let rec convert_str ori tar =
	let convert_char c = match c with
		'n' -> '\n'
		| 'r' -> '\r'
		| 't' -> '\t'
		| 'b' -> '\b'
		| '\'' -> '\''
		| '\"' -> '\"'
		| '\\' -> '\\'
		| _ -> raise (Failure ("Illegal escpae character: "^"\\"^(String.make 1 c)))
in
	match ori with
		"" -> tar
		| _ -> if (String.get ori 0) = '\\' then convert_str (String.sub ori 2 ((String.length ori) - 2)) (tar^(String.make 1 (convert_char (String.get ori 1)))) else convert_str (String.sub ori 1 ((String.length ori) - 1)) (tar^(String.make 1 (String.get ori 0)))
}

(* letter, digit *)
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let character = ['\b' '\t' '\n' '\r' '\\' ' ' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '-' '_' '+' '=' '{' '[' '}' ']' '|' ';' ':' '<' '>' '.' ',' '?' '/' '~' '`']
            | letter | digit

rule token = parse
    | "/*"                 { comment lexbuf }
    | "//"                 { line_comment lexbuf }
    | [' ' '\t' '\r' '\n'] { token lexbuf }

    (* Keywords *)
    | "if"            { IF }
    | "else"          { ELSE }
    | "while"         { WHILE }
    | "do"            { DO }
    | "for"           { FOR }
    | "break"         { BREAK }
    | "continue"      { CONTINUE }
    | "foreach"       { FOREACH }
    | "in"            { IN }
    | "by"            { BY }
    | "preorder"      { PREORDER }
    | "inorder"       { INORDER }
    | "postorder"      { POSTORDER }
    | "levelorder"    { LEVELORDER }
    | "return"        { RETURN }
    | "int"           { INT_T }
    | "float"         { FLOAT_T }
    | "char"          { CHAR_T }
    | "string"        { STRING_T }
    | "bool"          { BOOL_T }
    | "void"          { VOID }
    | "treetype"      { TREETYPE }
    | "main"          as main      
                      { ID(main) }
    | "print"         as print
                      { ID(print) }
    | "alloc"         as alloc
                      { ID(alloc) }
	
    (* Constants *)
    | digit+          as integer        
                      { INT(int_of_string integer) }
    | digit+ '.' digit* 
    | '.' digit+ ('e' ['+''-']? digit+)?
    | digit+ ('.' digit+)? 'e' ['+' '-']? digit+      as float
                      { FLOAT(float_of_string float) }
    | "true"
    | "false"         as bool
                      { BOOL(bool_of_string bool) }
    | '"' (character | '\'')* '"' as string 
                      { STRING(convert_str (String.sub string 1 ((String.length string) - 2)) "") }
    | '\'' (character | ''' | '"') '\'' as string
                      { CHAR(String.get string 1) }
    | '\'' '\\' 'n' '\'' 
                      { CHAR('\n') }
    | '\'' '\\' 't' '\'' 
                      { CHAR('\t') }
    | '\'' '\\' 'b' '\'' 
                      { CHAR('\b') }
    | '\'' '\\' 'r' '\'' 
                      { CHAR('\r') }
    | '\'' '\\' '\\' '\'' 
                      { CHAR('\\') }

    | '~'             { NULL }


    | '{'    { LBRACE }
    | '}'    { RBRACE }
    | ';'    { SEMI }
    | ':'    { COLON }
    | ','    { COMMA }

    | '='    { ASSIGN }
    
    (* Operators *)
    | "->"   { CONNECT }
    (* These operators are removed.
    | "+="   { PLUS_ASN }
    | "-="   { MINUS_ASN }
    | "*="   { TIMES_ASN }
    | "/="   { DIVIDE_ASN }
    | "%="   { MOD_ASN }
    *)
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
    | '^'    { FATHER }
    | '('    { LPAREN }
    | ')'    { RPAREN }
	
	| letter (letter | digit | '_')* as identifier 
				   { ID(identifier) }

    | eof { EOF }
    | _ as err_char { raise (Failure("illegal character " ^ Char.escaped err_char)) }

(* comment *)
and line_comment = parse
    '\n'     { token lexbuf }
    | _      { line_comment lexbuf }

and comment = parse
    "*/"     { token lexbuf }
    | _      { comment lexbuf }

