(* Higher level interface. *)

open Core.Std
open Ctypes


exception Crypto_failed

type t = char array

module Secretbox = struct
  type t = {
    arr : char array;
    n : char ptr;
    nlen : int;
    c : char ptr;
    clen : int
  }

  let char_ptr_of_string =
    coerce string (ptr char)

  let llong_of_int i =
    Signed.LLong.of_int64 (Int64.of_int i)

  let string_of_char_array ?(start=0) arr =
    let alen = Array.length arr in
    assert (start < alen);
    let slen = alen - start in
    String.init slen ~f:(fun i -> Array.get arr (i + start))

  let char_array_of_string s =
    let arrptr = char_ptr_of_string s in
    Array.from_ptr arrptr (String.length s)

  let create arr =
    let n = Array.start arr in
    let nlen = Libsodium.crypto_secretbox_NONCEBYTES in
    let c = n +@ nlen in
    let clen = (Array.length arr) - nlen in
    { arr; n; nlen; c; clen }

  let box key data =
    let padded_m = (String.make Libsodium.crypto_secretbox_ZEROBYTES '\000') ^ data in
    let clen = String.length padded_m in
    let nlen = Libsodium.crypto_secretbox_NONCEBYTES in
    let boxed = Array.make char (nlen + clen) in

    let n = Array.start boxed in
    Libsodium.randombytes_buf (to_voidp n) (Unsigned.Size_t.of_int nlen);

    let c = n +@ nlen in
    let m = char_ptr_of_string padded_m in
    let mlen = llong_of_int clen in
    let k = char_ptr_of_string key in
    if (Libsodium.crypto_secretbox c m mlen n k) = 0 then create boxed
    else raise Crypto_failed

  let box_open key boxed =
    let padded_m = Array.make char boxed.clen ~initial:'\000' in
    let m = Array.start padded_m in
    let k = char_ptr_of_string key in
    let clen = llong_of_int boxed.clen in
    if (Libsodium.crypto_secretbox_open m boxed.c clen boxed.n k) = 0 then
      string_of_char_array padded_m ~start:Libsodium.crypto_secretbox_ZEROBYTES
    else raise Crypto_failed

  let to_string boxed = string_of_char_array boxed.arr

  let of_string s = create (char_array_of_string s)
end
