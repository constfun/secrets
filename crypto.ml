open Ctypes

type t = char array

let prepare data =
  let m = (String.make 32 '\000') ^ data in
  let mlen = String.length m in
  let mlenLL = Signed.LLong.of_int64 (Int64.of_int mlen) in
  let c = Array.make char mlen in
  (c, m, mlen, mlenLL)

let encrypt data =
  let k = "01234567891011121314151617181920" in
  let n = "029181716151413121110198" in
  let c, m, _, mlen = prepare data in
  ignore (Sodium.crypto_secretbox (Array.start c) m mlen k n);
  c

let to_char_list = Array.to_list
