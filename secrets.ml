open Core.Std
open Entry
open Re2


type hl = (int, Int.comparator_witness) Set.t

type qres = {
  summary : string;
  summary_hl : hl;
  value: string;
}

module Secrets : sig
  type entry
  type t

  include Stringable with type t := t

  val create : unit -> t
  (*val parse : string -> t option*)

  (* XXX: Should Container.Make *)
  val add : t -> entry -> t
  val append : t -> t -> t

  val search : t -> string -> qres list
end = struct
  type entry = Entry.t
  type t = entry list

  let create () = []

  let add sec entry = entry :: sec
  let append = List.append

  let search sec query =
    let search_space = List.fold sec ~init:[] ~f:(fun acc e ->
      let prefix = Entry.title e ^ " " in
      List.fold (Entry.payload e) ~init:acc ~f:(fun acc (k, v) -> (prefix ^ k, v) :: acc)
    ) in
    let rs = String.concat_map query ~sep:".*?" ~f:(fun c -> "(" ^ String.of_char c ^ ")") in
    let r = Regex.create_exn rs in
    List.filter_map search_space ~f:(fun (summary, value) ->
      match Regex.get_matches ~max:1 r summary with
      | Ok r -> (match r with
        | m :: _ ->
            let summary_hl = ref (Set.empty Int.comparator) in
            let num_submatches = (String.length query) in
            for i = 1 to num_submatches do
              let (match_indx, len) = Regex.Match.get_pos_exn ~sub:(`Index i) m in
              summary_hl := Set.add !summary_hl match_indx
            done;
            Some { summary; summary_hl=(!summary_hl); value }
        | [] -> None
      )
      | Error e -> None
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
