open Core.Std

module type S = sig
  type entry = { title : string; payload : (string * string) list } with sexp
  type t = entry list with sexp

  val create : unit -> t
  (*val parse : string -> t option*)

  (* XXX: Should Container.Make *)
  val add : t -> entry -> t
  val append : t -> t -> t

  val to_string : t -> string
  val of_string : string -> t
end

module Secrets = struct
  type entry = { title : string; payload : (string * string) list } with sexp
  type t = entry list with sexp

  let create () = []

  let add sec entry = entry :: sec
  let append = List.append

  let to_string sec = Sexp.to_string (sexp_of_t sec)
  let of_string s = t_of_sexp (Sexp.of_string s)
end
