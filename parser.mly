%token <string> TITLE
%token <string * string> FIELD
%token NEWLINE
%token DOUBLE_NEWLINE
%token EOF
%start <(string * ((string * string) list)) list option> prog
%%
prog:
  | sec = separated_list(DOUBLE_NEWLINE, entry); EOF { Some sec }
  | EOF { None } ;
entry:
  t = TITLE; p = payload { (t, p) } ;
payload:
  p = nonempty_list(FIELD) { p } ;
