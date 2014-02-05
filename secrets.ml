open Core.Std
open Entry
open Re2


module Secrets : sig
  type entry
  type t

  include Stringable with type t := t

  val create : unit -> t
  (*val parse : string -> t option*)

  (* XXX: Should Container.Make *)
  val add : t -> entry -> t
  val append : t -> t -> t

  val search : t -> string -> string list
end = struct
  type entry = Entry.t
  type t = entry list

  let create () = []

  let add sec entry = entry :: sec
  let append = List.append

  let search sec query =
    let search_space = List.fold sec ~init:[] ~f:(fun acc e ->
      let prefix = Entry.title e ^ " " in
      List.fold (Entry.payload e) ~init:acc ~f:(fun acc (k, _) -> (e, prefix ^ k) :: acc)
    ) in
    let r = Regex.create_exn (String.concat_map query ~sep:".*?" ~f:String.of_char) in
    List.filter_map search_space ~f:(fun (e, s) ->
      match Regex.matches r s with
      | true -> Some s
      | false -> None
    )

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
