%token <string> TITLE
%start <string option> prog
%token EOF
%%
prog:
  | t = TITLE { Some t }
  | EOF { None }
  ;
