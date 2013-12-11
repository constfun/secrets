(* C equivalent interface. *)

open Ctypes
open Foreign


let crypto_secretbox_ZEROBYTES = 32
let crypto_secretbox_BOXZEROBYTES = 16
let crypto_secretbox_NONCEBYTES = 24


let crypto_secretbox =
  foreign "crypto_secretbox" (ptr char @-> ptr char @-> llong @-> ptr char @-> ptr char @-> returning int)

let randombytes_buf =
  foreign "randombytes_buf" (ptr void @-> size_t @-> returning void)
