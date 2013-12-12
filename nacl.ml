(* Higher level interface. *)

open Core.Std
open Ctypes


exception Crypto_failed

type t = char array

module Secretbox = struct
  type t = {
    buf : char array;
    n : char ptr;
    nlen : int;
    c : char ptr;
    clen : int
  }

  let create buf =
    let n = Array.start buf in
    let nlen = Libsodium.crypto_secretbox_NONCEBYTES in
    let c = n +@ nlen in
    let clen = (Array.length buf) - nlen in
    { buf; n; nlen; c; clen }

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

  let box key data =
    let k = char_ptr_of_string key in
    let m = (String.make Libsodium.crypto_secretbox_ZEROBYTES '\000') ^ data in
    let mptr = char_ptr_of_string m in
    let mlen = String.length m in
    let mlenLL = llong_of_int mlen in

    let buflen = Libsodium.crypto_secretbox_NONCEBYTES + mlen in
    let boxed = create (Array.make char buflen) in
    Libsodium.randombytes_buf (to_voidp boxed.n) (Unsigned.Size_t.of_int boxed.nlen);

    if (Libsodium.crypto_secretbox boxed.c mptr mlenLL boxed.n k) = 0 then boxed
    else raise Crypto_failed

  let box_open key boxed =
    let m = Array.make char boxed.clen ~initial:'\000' in
    let mptr = Array.start m in
    let k = char_ptr_of_string key in
    let clenLL = llong_of_int boxed.clen in
    if (Libsodium.crypto_secretbox_open mptr boxed.c clenLL boxed.n k) = 0 then
      string_of_char_array m ~start:Libsodium.crypto_secretbox_ZEROBYTES
    else raise Crypto_failed

  let to_string boxed = string_of_char_array boxed.buf

  let of_string s = create (char_array_of_string s)
end
