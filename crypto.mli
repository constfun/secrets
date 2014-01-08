type t

val create : string -> t
val with_file : string -> key:t -> f:(string -> string) -> unit
