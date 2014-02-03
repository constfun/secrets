open Core.Std
open Lexer
open Lexing
open Secrets_parser

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse s =
  let lexbuf = Lexing.from_string s in
  let e = try Secrets_parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Secrets_parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    None in
  match e with
  | Some l -> Some l
  | None -> None
