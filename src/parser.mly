/* parser.mly */
/* @authors: Shuai Sun,  */
/* guys, put your names here if you have composed the file */

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
%token PLUS_ASN MINUS_ASN TIMES_ASN DIVIDE_ASN MOD_ASN
%token OR AND NOT
%token NEQ GT LT LEQ GEQ EQ
%token PLUS MINUS TIMES DIVIDE MOD
%token DOLLAR AT LBRACK RBRACK DEG_AND DOT HASH FATHER
%token LPAREN RPAREN

%nonassoc NOELSE
%nonassoc ELSE

%right CONNECT
%right ASSIGN
%right PLUS_ASN MINUS_ASN
%right TIMES_ASN DEVIDE_ASN MOD_ASN 
%left BINOP
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
%type <int> program      /* this type should be AST.program. just put int because AST is not finished */
%%


program:
    /* nothing */                    { [] }
    | program type_def                    { [] }
    | program decl                    { [] }
    | program func_def                    { [] }
    
type_def:
    TREETYPE GT INT LT ID LBRACE decl_list RBRACE                    { [] }
    | TREETYPE GT INT COMMA LBRACK alias_list RBRACK LT ID LBRACE decl_list RBRACE                    { [] }

alias_list:
    ID                    { [] }
    | alias_list ID                    { [] }

decl_list:
    decl                    { [] }
    | decl_list decl                    { [] }

decl:
    type_specifier init_list SEMI                    { [] }

type_specifier:
    INT_T                     { [] }
    | FLOAT_T                     { [] }
    | CHAR_T                     { [] }
    | STRING_T                     { [] }
    | BOOL_T                     { [] }
    | ID                    { [] }
		| VOID                  { [] }

/*    
return_type:
    type_specifier                     { [] }
    | VOID                    { [] }
*/

init_list:
    init                    { [] }
    | init_list COMMA init                    { [] }

init:
    ID                    { [] }
    | ID ASSIGN expr                    { [] }

func_def:
    type_specifier ID LPAREN para_list RPAREN stmt_block                    { [] }

para_list:
    /* nothing */
    | para_decl                    { [] }
    | para_list COMMA para_decl                    { [] }

para_decl:
    type_specifier ID                    { [] }
    
stmt_block:
    LBRACE stmt_list RBRACE                    { [] }

stmt_list:
    /* nothing */
    | stmt_list stmt                    { [] }

stmt:
    expr SEMI                    { [] }
    | decl SEMI                    { [] }
    | stmt_block                     { [] }
    | IF LPAREN expr RPAREN stmt %prec NOELSE                    { [] }
    | IF LPAREN expr RPAREN stmt ELSE stmt                    { [] }
    | WHILE LPAREN expr RPAREN stmt                    { [] }
    | DO stmt WHILE LPAREN expr RPAREN SEMI                    { [] }
    | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt                    { [] }
    | FOREACH LPAREN ID IN ID RPAREN BY trvs_order stmt                    { [] }
    | BREAK SEMI                    { [] }
    | CONTINUE SEMI                    { [] }
    | RETURN expr SEMI                    { [] }
    | RETURN SEMI                    { [] }
    | SEMI                    { [] }

trvs_order:
    INORDER                     { [] }
    | PREORDER                     { [] }
    | POSTORDER                     { [] }
    | LEVELORDER                    { [] }

/* To construct the expr, there are two ways.
   The second causes more conflicts.
   The first is: */

/*
binop:
    PLUS                    { [] }
    | MINUS                    { [] }
    | TIMES                    { [] }
    | DIVIDE                    { [] }
    | MOD                    { [] }

    | GT                    { [] }
    | LT                    { [] }
    | GEQ                    { [] }
    | LEQ                    { [] }
    | NEQ                    { [] }
    | EQ                    { [] }

    | AND                    { [] }
    | OR                    { [] }

unop:
    PLUS                    { [] }
    | MINUS                    { [] }
    | AT                    { [] }
    | DOLLAR                    { [] }
    | FATHER                    { [] }
    | NOT                    { [] }
    | HASH                    { [] }
    | DEG_AND                    { [] }

tr_construct:
    lvalue CONNECT LPAREN node_list RPAREN                    { [] }

expr:
    lvalue                    { [] }
    | tr_construct                    { [] }
    | literal                    { [] }
    | expr binop expr %prec BINOP                    { [] }
    | unop expr                    { [] }
    | LPAREN expr RPAREN                    { [] }
    | lvalue ASSIGN expr                    { [] }
    | ID LPAREN arg_list RPAREN                    { [] }
*/    
    
/* End of the first.
    
    The second is: */
   
expr:
    | literal                    { [] }

    | expr PLUS expr                    { [] }
    | expr MINUS expr                     { [] }
    | expr TIMES expr                    { [] }
    | expr DIVIDE expr                    { [] }
    | expr MOD expr                    { [] }

    | expr GT expr                    { [] }
    | expr LT expr                    { [] }
    | expr GEQ expr                    { [] }
    | expr LEQ expr                    { [] }
    | expr NEQ expr                    { [] }
    | expr EQ expr                    { [] }

    | expr AND expr                    { [] }
    | expr OR expr                    { [] }

    | PLUS expr                    { [] }
    | MINUS expr                    { [] }
    | AT expr                    { [] }
    | DOLLAR expr                    { [] }
    | FATHER expr                    { [] }
    | NOT expr                    { [] }
    | HASH expr                    { [] }
    | DEG_AND expr                    { [] }

    | lvalue                    { [] }

    | lvalue ASSIGN expr                    { [] }
    
    | lvalue CONNECT LPAREN node_list RPAREN                    { [] }
    
    | LPAREN expr RPAREN                    { [] }
    | ID LPAREN arg_list RPAREN                    { [] }

    
/* End of the second */

literal:
    INT                    { [] }
    | FLOAT                    { [] }
    | STRING                    { [] }
    | CHAR                    { [] }
    | BOOL                    { [] }
    | NULL                    { [] }

node_list:
    expr                    { [] }
    | node_list COLON expr                    { [] }

lvalue:
    ID                    { [] }
    | expr DOT ID                    { [] }
    | expr LBRACK expr RBRACK                    { [] }

arg_list:
    /* nothing */                    { [] }
    | expr                    { [] }
    | arg_list COMMA expr                    { [] }
    
    















