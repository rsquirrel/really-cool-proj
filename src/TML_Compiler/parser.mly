/* parser.mly */
/* @authors: Shuai Sun, Yan Zou, Jiabin Hu, Akash */
%{ open Type %}
%{ open Ast %}

%token <string> ID
%token IF ELSE WHILE DO FOR BREAK CONTINUE FOREACH IN BY RETURN
%token PREORDER INORDER POSTORDER LEVELORDER
%token INT_T FLOAT_T CHAR_T STRING_T BOOL_T VOID TREETYPE
%token <int>INT
%token <float>FLOAT
%token <bool>BOOL
%token <string>STRING
%token <char>CHAR
%token NULL
%token LBRACE RBRACE SEMI COLON COMMA
%token ASSIGN
%token CONNECT 
/* These operators are removed
%token PLUS_ASN MINUS_ASN TIMES_ASN DIVIDE_ASN MOD_ASN
*/
%token OR AND NOT
%token NEQ GT LT LEQ GEQ EQ
%token PLUS MINUS TIMES DIVIDE MOD
%token DOLLAR AT LBRACK RBRACK DEG_AND DOT HASH FATHER
%token LPAREN RPAREN
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%right CONNECT
%right ASSIGN
/* These operators are removed
%right PLUS_ASN MINUS_ASN
%right TIMES_ASN DEVIDE_ASN MOD_ASN
*/ 
%left OR 
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS   /* for binop they are left, but for unop should they be right? */
%right SIGN
%left TIMES DIVIDE MOD
%right NOT
%right AT, DOLLAR, FATHER
%right HASH, DEG_AND
%nonassoc LBRACK
%left DOT

%start program
%type <Ast.program> program      /* this type should be AST.program. just put int because AST is not finished */
%%


program:
    /* nothing */                             { [] }   /* this part should be differnt 
                                                       and it should be similar to MicroC unsure about it */
    | program type_def                        { Treedef($2)::$1 }
    | program decl                            { Globalvar($2)::$1 }
    | program func_def                        { Funcdef($2)::$1 }
    
type_def:
    TREETYPE LT INT GT ID LBRACE decl_list RBRACE                                      
                                                            { { typename = $5;
                                                                members = List.rev $7;
                                                                degree = $3;
                                                                aliases = [] }}
    | TREETYPE LT INT COMMA LBRACK alias_list RBRACK GT ID LBRACE decl_list RBRACE     
                                                            { { typename = $9;
                                                                members = List.rev $11;
                                                                degree = $3;
                                                                aliases = List.rev $6 } }

alias_list:
    ID                                       { [$1] }
    | alias_list COMMA ID                    { $3::$1 }

decl_list:
    decl                                     { [$1] }
    | decl_list decl                         { $2::$1 }

decl:
    type_specifier init_list SEMI            { ($1, List.rev $2) }

type_specifier:
    INT_T                                   { Int }
    | FLOAT_T                               { Float }
    | CHAR_T                                { Char }
    | STRING_T                              { String }
    | BOOL_T                                { Boolean }
    | ID                                    { Tree_type($1) }       
		| VOID                                  { Void }

/*    
return_type:
    type_specifier                          { $1 }
    | VOID                                  { Void }
*/

init_list:
    init                                    { [$1] }
    | init_list COMMA init                  { $3::$1 }

init:
    ID                                      { WithoutInit($1) }
    | ID ASSIGN expr                        { WithInit($1 ,$3) }

func_def:
    type_specifier ID LPAREN para_list RPAREN stmt_block           { {  return_type = $1;
                                                                        fname = $2;
                                                                        params = List.rev $4;
																	  	body = $6	
																    } }

para_list:
    /* nothing */					        {[]}
    | para_decl                             { [$1] }
    | para_list COMMA para_decl             { $3::$1 }

para_decl:
    type_specifier ID                       { ($1, $2) }
    
stmt_block:
    LBRACE stmt_list RBRACE                 { List.rev $2 }

stmt_list:
    /* nothing */                           { [] }
    | stmt_list stmt                        { $2::$1 }

stmt:
    expr SEMI                               				  { Expr($1) }
    | decl                                                    { Vardecl($1) }
    | stmt_block                                              { Block($1) }
    | IF LPAREN expr RPAREN stmt %prec NOELSE                 { If($3, $5, Empty) }
    | IF LPAREN expr RPAREN stmt ELSE stmt                    { If($3, $5, $7) }
    | WHILE LPAREN expr RPAREN stmt                           { While($3, $5) }
    | DO stmt WHILE LPAREN expr RPAREN SEMI                   { Do($2, $5) }
    | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt         { For($3, $5, $7, $9) }
    /*| FOREACH LPAREN ID IN expr BY trvs_order RPAREN stmt     { Foreach($3, $5, $7, $9) } */
    | FOREACH ID IN expr BY trvs_order stmt                   { Foreach($2, $4, $6, $7) } 
                                                                            /* drop the parenthesis */
    | BREAK SEMI                                              { Break }
    | CONTINUE SEMI                                           { Continue }
    | RETURN expr SEMI                                        { Return($2) }
    | RETURN SEMI                                             { ReturnVoid }
    | SEMI                                                    { Empty }      /*No action really.. ?*/

trvs_order:
    INORDER                                               { Inorder }    
                                                            /*I doubt what exactly these should contain..?*/
    | PREORDER                                            { Preorder }
    | POSTORDER                                           { Postorder }
    | LEVELORDER                                          { Levelorder }

expr:
    | literal                             { Literal($1) }

    | expr PLUS expr                      { Binop($1, Add, $3) }
    | expr MINUS expr                     { Binop($1, Sub, $3) }
    | expr TIMES expr                     { Binop($1, Mult, $3) }
    | expr DIVIDE expr                    { Binop($1, Div, $3) }
    | expr MOD expr                       { Binop($1, Mod, $3) }

    | expr GT expr                    	  { Binop($1, Greater_than, $3) }
    | expr LT expr                        { Binop($1, Less_than, $3) }
    | expr GEQ expr                       { Binop($1, Geq, $3) }
    | expr LEQ expr                       { Binop($1, Leq, $3) }
    | expr NEQ expr                       { Binop($1, Neq, $3) }
    | expr EQ expr                        { Binop($1, Equal, $3) }

    | expr AND expr                       { Binop($1, And, $3) }
    | expr OR expr                        { Binop($1, Or, $3) }

    | PLUS expr                           { Uniop(Add, $2) }
    | MINUS expr                          { Uniop(Sub, $2 ) }
    | AT expr                    		  { Uniop(At, $2) }
    | DOLLAR expr                         { Uniop(Dollar, $2) }
    | FATHER expr                         { Uniop(Father, $2) }
    | NOT expr                            { Uniop(Not, $2) }
    | HASH expr                           { Uniop(Hsh, $2) }
    | DEG_AND expr                        { Uniop(Deg_a, $2) }

    | lvalue                              { $1 }

    | lvalue ASSIGN expr                  { Assign($1, $3) }
    
    | lvalue CONNECT LPAREN node_list RPAREN  { Conn($1, List.rev  $4) }
    
    | LPAREN expr RPAREN                  { $2 }
    | ID LPAREN arg_list RPAREN           { Call($1, List.rev $3) }


literal:
    INT                                   { IntLit($1) }
    | FLOAT                               { FloatLit($1) }
    | STRING                              { StringLit($1) }
    | CHAR                                { CharLit($1) }
    | BOOL                                { BoolLit($1) }
    | NULL                                { TreeLit }         
                                            /*We dont have a NULL type defined in ast yet*/

node_list:
    expr                                  { [$1] }
    | node_list COLON expr                { $3::$1 }

lvalue:
    ID                                    { Id($1) }
    | expr DOT ID                         { Binop($1, Dot, Id($3)) }
    | expr LBRACK expr RBRACK             { Binop($1, Child, $3) }

arg_list:
    /* nothing */                         { [] }
    | expr                                { [$1] }
    | arg_list COMMA expr                 { $3::$1 }
   
