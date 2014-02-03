type t
type entry

val create : unit -> t
val parse : string -> t option

(* XXX: Should Container.Make *)
val add : t -> entry -> t
val append : t -> t -> t

val to_string : t -> string
val of_string : string -> t
