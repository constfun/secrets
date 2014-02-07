open Core.Std

type t = string

let create path =
    match Sys.file_exists_exn path with
    | true -> In_channel.read_all path
    | false ->
        let key = Nacl.randombytes 32 in
        Out_channel.with_file path ~perm:0o700 ~f:(fun oc -> Out_channel.output_string oc key);
        key

let encrypt key s = Nacl.Secretbox.to_string (Nacl.Secretbox.box key s)
let decrypt key s = Nacl.Secretbox.box_open key (Nacl.Secretbox.of_string s)

let with_file path ~key ~f =
    let contents = match Sys.file_exists_exn path with
    | true -> decrypt key (In_channel.read_all path)
    | false -> "" in
    Out_channel.write_all path ~data:(encrypt key (f contents))
