external randombytes : int -> string = "nacl_randombytes"

module Secretbox = struct
  let crypto_secretbox_NONCEBYTES = 24

  type t = { nonce : string; cyphertext : string }

  external nacl_secretbox : string -> string -> string -> string = "nacl_secretbox"

  let box key data =
    let nonce = randombytes crypto_secretbox_NONCEBYTES in
    let cyphertext = nacl_secretbox data nonce key in
    { nonce; cyphertext }

  let to_string t = t.nonce ^ t.cyphertext
end
