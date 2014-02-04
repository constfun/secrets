open Core.Std

open Parser
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse s =
  let lexbuf = Lexing.from_string s in
  print_string s;
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg as e ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    raise e
  | Parser.Error as e ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    raise e
