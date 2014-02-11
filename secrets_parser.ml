open Core.Std

open Parser
open Lexer
open Lexing
open Textutils

exception Error

let print_position lexbuf =
  let buf = lexbuf.lex_buffer in
  let sp = lexbuf.lex_start_p in
  let cp = lexbuf.lex_curr_p in
  let line = String.slice buf sp.pos_bol (cp.pos_cnum-1) in
  let lnum = Int.to_string cp.pos_lnum in
  Console.Ansi.eprintf [`Dim] "%s " lnum;
  fprintf stderr "%s" line;
  Console.Ansi.eprintf [`Red] " \204\173\n"

let parse s =
  let lexbuf = Lexing.from_string s in
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError _ ->
    print_position lexbuf;
    raise Error
  | Parser.Error ->
    print_position lexbuf;
    raise Error
