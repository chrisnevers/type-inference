%{
  open Ast
%}

%token EOF
%token FUN
%token ARROW
%token LPAREN RPAREN
%token LET IN
%token TRUE FALSE
%token EQ
%token APP
%token ADD
%token SUB
%token MUL
%token DIV
%token AND
%token OR
%token <int> NUM
%token <string> ID

%nonassoc LPAREN ID NUM TRUE FALSE
%left APP
%left AND OR
%left ADD SUB
%left MUL DIV

%start main

%type <Ast.exp> main

%%

main:
  | e=exp EOF                       { e }

exp:
  | ID                              { Var ($1) }
  | TRUE                            { Lit (Bool true) }
  | FALSE                           { Lit (Bool false) }
  | NUM                             { Lit (Int ($1)) }
  | exp ADD exp                     { Binop ($1, Add, $3) }
  | exp SUB exp                     { Binop ($1, Sub, $3) }
  | exp MUL exp                     { Binop ($1, Mul, $3) }
  | exp DIV exp                     { Binop ($1, Div, $3) }
  | exp AND exp                     { Binop ($1, And, $3) }
  | exp OR exp                      { Binop ($1, Or, $3) }
  | LET ID EQ exp IN exp            { Let ($2, $4, $6) }
  | FUN ID ARROW exp                { Abs ($2, $4) }
  | LPAREN exp RPAREN               { $2 }
  | exp exp %prec APP               { App ($1, $2) }

