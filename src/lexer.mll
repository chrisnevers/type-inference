{
    open Parser
}

rule token = parse
  | [' ''\r''\t''\n'] { token lexbuf }
  | "let"           { LET }
  | "in"            { IN }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "\\"            { FUN }
  | "+"             { ADD }
  | "-"             { SUB }
  | "*"             { MUL }
  | "/"             { DIV }
  | "&&"            { AND }
  | "||"            { OR }
  | ['0'-'9']+ as n { NUM (int_of_string n) }
  | ['a'-'z''A'-'Z']+ as id { ID id }
  | "->"            { ARROW }
  | "="             { EQ }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "#"             { comment lexbuf }
  | eof             { EOF }

and comment = parse
  | '\n'            { token lexbuf }
  | _               { comment lexbuf }
