%token <string> TITLE
%token <string * string> FIELD
%token NEWLINE
%token EOF
%start <Entry.Entry.t list> prog
%%

prog:
  NEWLINE*; sec = separated_list(entry_separator, entry); EOF { sec }
entry_separator:
  NEWLINE; NEWLINE { }
entry:
  t = TITLE; p = payload { Entry.Entry.create t p }
payload:
  p = nonempty_list(FIELD) { p }
