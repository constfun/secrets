let title = _*

rule read =
  parse
  | title { STRING (Lexing.lexeme lexbuf) }
  | eof { EOF }
