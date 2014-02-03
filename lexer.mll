{
  open Parser

  exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\n'
let key = [^ '\n' ':']+
let str = [^ '\n']+

rule read =
  parse
  | newline newline { DOUBLE_NEWLINE }
  | (key as key) ':' white? (str as value) newline? { FIELD (key, value) }
  | (str as title) newline { TITLE title }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
