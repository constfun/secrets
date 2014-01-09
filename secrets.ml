open Core.Std

type entry = { title : string; payload : (string * string) list } with sexp
type t = entry list with sexp

let create () = []

let parse s =
  let lexbuf = Lexing.from_string s in
  match Parser.prog Lexer.read lexbuf with
  | Some t -> print_endline ("title: " ^ t)
  | None -> ()

let to_string sec = Sexp.to_string (sexp_of_t sec)
let of_string s = t_of_sexp (Sexp.of_string s)
