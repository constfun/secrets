exception Nacl_error
let _ = Callback.register_exception "Nacl_error" Nacl_error


external randombytes : int -> string = "nacl_randombytes"


module Secretbox = struct
  let crypto_secretbox_NONCEBYTES = 24

  type t = { nonce : string; cyphertext : string }

  external nacl_secretbox : string -> string -> string -> string = "nacl_secretbox"
  external nacl_secretbox_open : string -> string -> string -> string = "nacl_secretbox_open"

  let box key data =
    let nonce = randombytes crypto_secretbox_NONCEBYTES in
    let cyphertext = nacl_secretbox data nonce key in
    { nonce; cyphertext }

  let box_open key boxed =
    print_endline boxed.cyphertext;
    print_endline boxed.nonce;
    nacl_secretbox_open boxed.cyphertext boxed.nonce key

  let to_string boxed = boxed.nonce ^ boxed.cyphertext
end
