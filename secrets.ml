open Core.Std
open Re2
open Crypto_intf

include Secrets_intf

module Make (C : Crypto_intf) : (Secrets_intf with type key = C.Key.t) = struct

  type key = C.Key.t
  type entry = { title : string; payload : (string * string) list } with sexp
  type db = entry list with sexp
  type t = { key : key; path : string; db : db }

  let decode s = Sexp.of_string s |> db_of_sexp
  let encode sec = sexp_of_db sec |> Sexp.to_string

  let save sec =
    let data = sexp_of_db sec.db
    |> Sexp.to_string
    |> C.encrypt sec.key
    |> C.to_string in
    Out_channel.write_all sec.path ~data

  let create key path =
    let sec = { key; path; db=[] } in
    match Sys.file_exists_exn ~follow_symlinks:true path with
    | true ->
      { sec with db = In_channel.read_all path
        |> C.of_string
        |> C.decrypt key
        |> decode
      }
    | false -> save sec; sec

  let with_file path ~key ~f =
    let sec = create key path in
    f sec;
    save sec
end

module NaclSecrets = Make(Crypto.NaclCrypto)
