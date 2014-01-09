%token <string> TITLE
%start <string option> prog
%%
prog:
  | t = TITLE { Some t }
  ;
