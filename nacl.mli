type t

exception Crypto_failed

val secretbox : string -> string -> t

val to_char_list : t -> char list
val of_char_list : char list -> t
