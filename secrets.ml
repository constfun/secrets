open Core.Std
open Entry


module Secrets : sig
  type entry
  type t

  include Stringable with type t := t

  val create : unit -> t
  (*val parse : string -> t option*)

  (* XXX: Should Container.Make *)
  val add : t -> entry -> t
  val append : t -> t -> t
end = struct
  type entry = Entry.t
  type t = entry list

  let create () = []

  let add sec entry = entry :: sec
  let append = List.append

  let of_string s =
    Secrets_parser.parse s

  let to_string sec =
    let open Bigbuffer in (
      let buff = create 256 in
      List.iter sec ~f:(fun e ->
        add_string buff (Entry.to_string e);
        add_string buff "\n\n"
      );
      contents buff
    )

end
