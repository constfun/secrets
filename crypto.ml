open Core.Std

include Crypto_intf

module NaclCrypto = struct
  type t = Nacl.Secretbox.t

  module Key = struct
    type t = string

    let create path =
      match Sys.file_exists_exn ~follow_symlinks:true path with
      | true -> In_channel.read_all path
      | false ->
         let key = Nacl.randombytes 32 in
         Out_channel.with_file path ~perm:0o700 ~f:(fun oc -> Out_channel.output_string oc key);
         key

    let to_string k = k
  end

  let encrypt key data = Nacl.Secretbox.box (Key.to_string key) data
  let decrypt key data = Nacl.Secretbox.box_open (Key.to_string key) data

  let to_string = Nacl.Secretbox.to_string
  let of_string = Nacl.Secretbox.of_string
end
