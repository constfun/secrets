{
  open Parser
}

let title = _*

rule read =
  parse
  | title { TITLE (Lexing.lexeme lexbuf) }
  | eof { EOF }
