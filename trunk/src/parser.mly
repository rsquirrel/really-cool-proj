/* parser.mly */
/* @authors: Shuai Sun, Yan Zou, Jiabin Hu, Akash */

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
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%right CONNECT
%right ASSIGN
%right PLUS_ASN MINUS_ASN
%right TIMES_ASN DEVIDE_ASN MOD_ASN 
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
    /* nothing */                    { 0 }
    | program type_def                    { 0 }
    | program decl                    { 0 }
    | program func_def                    { 0 }
    
type_def:
    TREETYPE LT INT GT ID LBRACE decl_list RBRACE                    { 0 }
    | TREETYPE LT INT COMMA LBRACK alias_list RBRACK GT ID LBRACE decl_list RBRACE                    { 0 }

alias_list:
    ID                    { 0 }
    | alias_list COMMA ID                    { 0 }

decl_list:
    decl                    { 0 }
    | decl_list decl                    { 0 }

decl:
    type_specifier init_list SEMI                    { 0 }

type_specifier:
    INT_T                     { 0 }
    | FLOAT_T                     { 0 }
    | CHAR_T                     { 0 }
    | STRING_T                     { 0 }
    | BOOL_T                     { 0 }
    | ID                    { 0 }
		| VOID                  { 0 }

/*    
return_type:
    type_specifier                     { 0 }
    | VOID                    { 0 }
*/

init_list:
    init                    { 0 }
    | init_list COMMA init                    { 0 }

init:
    ID                    { 0 }
    | ID ASSIGN expr                    { 0 }

func_def:
    type_specifier ID LPAREN para_list RPAREN stmt_block                    { 0 }

para_list:
    /* nothing */					{ 0 }
    | para_decl                    { 0 }
    | para_list COMMA para_decl                    { 0 }

para_decl:
    type_specifier ID                    { 0 }
    
stmt_block:
    LBRACE stmt_list RBRACE                    { 0 }

stmt_list:
    /* nothing */						{ 0 }
    | stmt_list stmt                    { 0 }

stmt:
    expr SEMI                    { 0 }
    | decl SEMI                    { 0 }
    | stmt_block                     { 0 }
    | IF LPAREN expr RPAREN stmt %prec NOELSE                    { 0 }
    | IF LPAREN expr RPAREN stmt ELSE stmt                    { 0 }
    | WHILE LPAREN expr RPAREN stmt                    { 0 }
    | DO stmt WHILE LPAREN expr RPAREN SEMI                    { 0 }
    | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt                    { 0 }
    | FOREACH LPAREN ID IN ID RPAREN BY trvs_order stmt                    { 0 }
    | BREAK SEMI                    { 0 }
    | CONTINUE SEMI                    { 0 }
    | RETURN expr SEMI                    { 0 }
    | RETURN SEMI                    { 0 }
    | SEMI                    { 0 }

trvs_order:
    INORDER                     { 0 }
    | PREORDER                     { 0 }
    | POSTORDER                     { 0 }
    | LEVELORDER                    { 0 }

/* To construct the expr, there are two ways.
   The second causes more conflicts.
   The first is: */

/*
binop:
    PLUS                    { 0 }
    | MINUS                    { 0 }
    | TIMES                    { 0 }
    | DIVIDE                    { 0 }
    | MOD                    { 0 }

    | GT                    { 0 }
    | LT                    { 0 }
    | GEQ                    { 0 }
    | LEQ                    { 0 }
    | NEQ                    { 0 }
    | EQ                    { 0 }

    | AND                    { 0 }
    | OR                    { 0 }

unop:
    PLUS                    { 0 }
    | MINUS                    { 0 }
    | AT                    { 0 }
    | DOLLAR                    { 0 }
    | FATHER                    { 0 }
    | NOT                    { 0 }
    | HASH                    { 0 }
    | DEG_AND                    { 0 }

tr_construct:
    lvalue CONNECT LPAREN node_list RPAREN                    { 0 }

expr:
    lvalue                    { 0 }
    | tr_construct                    { 0 }
    | literal                    { 0 }
    | expr binop expr %prec BINOP                    { 0 }
    | unop expr                    { 0 }
    | LPAREN expr RPAREN                    { 0 }
    | lvalue ASSIGN expr                    { 0 }
    | ID LPAREN arg_list RPAREN                    { 0 }
*/    
    
/* End of the first.
    
    The second is: */
   
expr:
    | literal                    { 0 }

    | expr PLUS expr                    { 0 }
    | expr MINUS expr                     { 0 }
    | expr TIMES expr                    { 0 }
    | expr DIVIDE expr                    { 0 }
    | expr MOD expr                    { 0 }

    | expr GT expr                    { 0 }
    | expr LT expr                    { 0 }
    | expr GEQ expr                    { 0 }
    | expr LEQ expr                    { 0 }
    | expr NEQ expr                    { 0 }
    | expr EQ expr                    { 0 }

    | expr AND expr                    { 0 }
    | expr OR expr                    { 0 }

    | PLUS expr                    { 0 }
    | MINUS expr                    { 0 }
    | AT expr                    { 0 }
    | DOLLAR expr                    { 0 }
    | FATHER expr                    { 0 }
    | NOT expr                    { 0 }
    | HASH expr                    { 0 }
    | DEG_AND expr                    { 0 }

    | lvalue                    { 0 }

    | lvalue ASSIGN expr                    { 0 }
    
    | lvalue CONNECT LPAREN node_list RPAREN                    { 0 }
    
    | LPAREN expr RPAREN                    { 0 }
    | ID LPAREN arg_list RPAREN                    { 0 }

    
/* End of the second */

literal:
    INT                    { 0 }
    | FLOAT                    { 0 }
    | STRING                    { 0 }
    | CHAR                    { 0 }
    | BOOL                    { 0 }
    | NULL                    { 0 }

node_list:
    expr                    { 0 }
    | node_list COLON expr                    { 0 }

lvalue:
    ID                    { 0 }
    | expr DOT ID                    { 0 }
    | expr LBRACK expr RBRACK                    { 0 }

arg_list:
    /* nothing */                    { 0 }
    | expr                    { 0 }
    | arg_list COMMA expr                    { 0 }
    
    















