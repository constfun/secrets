open Ctypes
open Foreign


let crypto_secretbox =
  foreign "crypto_secretbox" (ptr char @-> string @-> llong @-> string @-> string @-> returning int)
