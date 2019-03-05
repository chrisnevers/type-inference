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
%token <int> NUM
%token <string> ID

%nonassoc LPAREN ID NUM TRUE FALSE
%left APP

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
  | LET ID EQ exp IN exp            { Let ($2, $4, $6) }
  | FUN ID ARROW exp                { Abs ($2, $4) }
  | LPAREN exp RPAREN               { $2 }
  | exp exp %prec APP               { App ($1, $2) }

