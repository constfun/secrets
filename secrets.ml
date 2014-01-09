open Core.Std
open Lexer
open Lexing

type entry = { title : string; payload : (string * string) list } with sexp
type t = entry list with sexp

let create () = []

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse s =
  let lexbuf = Lexing.from_string s in
  let e = try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    None in
  match e with
  | Some (t, p) ->
      print_endline t;
      List.iter p ~f:(fun (k, v) -> printf "key %s value %s\n" k v)
  | None -> ()

let to_string sec = Sexp.to_string (sexp_of_t sec)
let of_string s = t_of_sexp (Sexp.of_string s)
