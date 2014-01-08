exception Nacl_error

val randombytes : int -> string

module Secretbox : sig
  type t

  val box : string -> string -> t
  val box_open : string -> t -> string
  val to_string : t -> string
  val of_string : string -> t
end
