exception Crypto_failed

type t

val randombytes_buf : int -> t

module Secretbox : sig
  type t
  val box : string -> string -> t
  val box_open : string -> t -> string
  val to_list : t -> char list
  val of_list : char list -> t
end
