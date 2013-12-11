(* Higher level interface. *)

open Core.Std
open Ctypes
open Libsodium


exception Crypto_failed


type t = {
  n : char array;
  c : char array;
}


let secretbox key data =
  let n = Array.make char crypto_secretbox_NONCEBYTES ~initial:'\000' in
  let nptr = Array.start n in
  let nsize = Unsigned.Size_t.of_int (Array.length n) in
  randombytes_buf (to_voidp nptr) nsize;

  let m = (String.make crypto_secretbox_ZEROBYTES '\000') ^ data in
  let mlen = String.length m in
  let mlenLL = Signed.LLong.of_int64 (Int64.of_int mlen) in

  let c = Array.make char mlen in
  let cptr = Array.start c in

  if (crypto_secretbox cptr m mlenLL key nptr) = 0 then { n; c }
  else raise Crypto_failed


let to_char_list t =
  (Array.to_list t.n) @ (Array.to_list t.c)


let of_char_list l =
  let nl, cl = List.split_n l crypto_secretbox_NONCEBYTES in
  { n=(Array.of_list char nl); c=(Array.of_list char cl) }
