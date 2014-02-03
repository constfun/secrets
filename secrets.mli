type t
type entry

val create : unit -> t
val parse : string -> entry option
val add : t -> entry -> t

val to_string : t -> string
val of_string : string -> t
