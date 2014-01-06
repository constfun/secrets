open Core.Std
open Core_extended.Std

type t = string

let create path =
  match Sys.file_exists_exn ~follow_symlinks:true path with
  | true -> In_channel.read_all path
  | false ->
     let key = Nacl.randombytes 32 in
     Out_channel.with_file path ~perm:0o700 ~f:(fun oc -> Out_channel.output_string oc key);
     key

let to_string k = k
