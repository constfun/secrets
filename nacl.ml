(* Higher level interface. *)

open Core.Std
open Ctypes


exception Crypto_failed

type t = char array

let randombytes_buf size =
    let buf = Array.make char size ~initial:'\000' in
    let buf_ptr = to_voidp (Array.start buf) in
    let buf_size = Unsigned.Size_t.of_int (Array.length buf) in
    Libsodium.randombytes_buf buf_ptr buf_size;
    buf

let char_ptr_of_string = coerce string (ptr char)

module Secretbox = struct
  type t = {
    nonce : char array;
    padded_cyphertext : char array;
  }

  let llong_of_int i =
    Signed.LLong.of_int64 (Int64.of_int i)

  let string_of_char_array ?(start=0) arr =
    let alen = Array.length arr in
    assert (start < alen);
    let slen = alen - start in
    String.init slen ~f:(fun i -> Array.get arr (i + start))

  let box key data =
    let nonce = randombytes_buf Libsodium.crypto_secretbox_NONCEBYTES in
    let padded_m = (String.make Libsodium.crypto_secretbox_ZEROBYTES '\000') ^ data in
    let padded_mlen = String.length padded_m in
    let padded_cyphertext = Array.make char padded_mlen in

    let c = Array.start padded_cyphertext in
    let m = char_ptr_of_string padded_m in
    let mlen = llong_of_int padded_mlen in
    let k = char_ptr_of_string key in
    let n = Array.start nonce in
    if (Libsodium.crypto_secretbox c m mlen n k) = 0 then { nonce; padded_cyphertext }
    else raise Crypto_failed

  let box_open key boxed =
    let padded_clen = Array.length boxed.padded_cyphertext in
    let padded_message = Array.make char padded_clen ~initial:'\000' in

    let m = Array.start padded_message in
    let c = Array.start boxed.padded_cyphertext in
    let clen = llong_of_int padded_clen in
    let n = Array.start boxed.nonce in
    let k = char_ptr_of_string key in
    if (Libsodium.crypto_secretbox_open m c clen n k) = 0 then
      string_of_char_array padded_message ~start:Libsodium.crypto_secretbox_ZEROBYTES
    else raise Crypto_failed

  let to_string boxed =
    (string_of_char_array boxed.nonce) ^ (string_of_char_array boxed.padded_cyphertext)

  let of_string s =
    let sptr = char_ptr_of_string s in
    let nlen = Libsodium.crypto_secretbox_NONCEBYTES in
    let clen = (String.length s) - nlen in
    let nonce = Array.from_ptr sptr nlen in
    let padded_cyphertext = Array.from_ptr (sptr +@ nlen) clen in
    { nonce; padded_cyphertext }

  let to_list boxed =
    (Array.to_list boxed.nonce) @ (Array.to_list boxed.padded_cyphertext)

  let of_list l =
    let nl, cl = List.split_n l Libsodium.crypto_secretbox_NONCEBYTES in
    { nonce=(Array.of_list char nl); padded_cyphertext=(Array.of_list char cl) }
end


