%token <string> TITLE
%token <string * string> FIELD
%token NEWLINE
%token EOF
%start <(string * ((string * string) list)) option> prog
%%
prog:
  | e = entry; EOF { Some e }
  | EOF { None } ;
entry:
  t = TITLE; p = payload { (t, p) } ;
payload:
  p = nonempty_list(FIELD) { p } ;
