open Core.Std
open Re2


type entry = { title : string; payload : (string * string) list } with sexp
type entry_with_password = { entry : entry; password : string }
type 'a db = 'a list with sexp

type 'a t = { key : string; path : string; db : 'a db }

let decode s = Sexp.of_string s |> db_of_sexp entry_of_sexp
let encode sec = sexp_of_db sexp_of_entry sec |> Sexp.to_string

let save sec =
  let data = sexp_of_db sexp_of_entry sec.db
  |> Sexp.to_string
  |> Nacl.Secretbox.box sec.key
  |> Nacl.Secretbox.to_string in
  Out_channel.write_all sec.path ~data

let create key path =
  let key = Key.to_string key in
  let sec = { key; path; db=[] } in
  match Sys.file_exists_exn ~follow_symlinks:true path with
  | true ->
    { sec with db = In_channel.read_all path
      |> Nacl.Secretbox.of_string
      |> Nacl.Secretbox.box_open key
      |> decode
    }
  | false -> save sec; sec

let with_file path ~key ~f =
  let sec = create key path in
  f sec;
  save sec
