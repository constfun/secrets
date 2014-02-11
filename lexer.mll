{
  open Lexing
  open Parser

  exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\n'
let key = [^ '\n' ':']+
let str = [^ '\n']+

rule read =
  parse
  | (key as key) ':' white? (str as value) (newline? as nl)
    {
      if nl = "" then new_line lexbuf;
      FIELD (key, value)
    }
  | (str as title) newline { new_line lexbuf; TITLE title }
  | newline* eof { EOF }
  | newline { new_line lexbuf; NEWLINE }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
