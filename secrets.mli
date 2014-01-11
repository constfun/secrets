type t
type entry

val create : unit -> t
val parse : string -> unit

val to_string : t -> string
val of_string : string -> t
