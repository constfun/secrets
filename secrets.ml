open Core.Std
open Re2

(* Proper way. *)
(*module Db : sig
  type 'a t
  val empty : 'a t
  val add : 'a t -> 'a -> 'a t
  val to_list : 'a t -> 'a list
end = struct
  type 'a t = 'a list
  let empty = []
  let add db entry = entry :: db
  let to_list db = db
end*)

(* Ghetto way. *)
type entry = { title : string; payload : (string * string) list; }
type 'a db = 'a list

let list () =
  print_string "Listing\n"

let import file =
  let r = Regex.create_exn "(^[^[].*)[\\s](.*)$" in
  let db = In_channel.fold_lines file ~init:[] ~f:(fun db l ->
    match Regex.find_submatches r l with
    | Error e -> db
    | Ok submatches ->
      let title = match submatches.(1) with Some(t) -> t | None -> assert false in
      let password = match submatches.(2) with Some(p) -> p | None -> assert false in
      let entry = { title; payload=[("password", password)] } in
      entry :: db
  ) in
  List.iter db ~f:(fun entry -> print_endline entry.title)

let existing_file =
  Command.Spec.Arg_type.create
    (fun filename -> match Sys.is_file filename with
    | `Yes -> filename
    | `No | `Unknown ->
      eprintf "'%s' file not found.\n%!" filename;
      exit 1
    )

let import_command =
  Command.basic ~summary:"Import entries from old format."
    Command.Spec.(
      empty
      +> anon (maybe ("filename" %: existing_file))
    )
    (fun filename () -> match filename with
    | Some(filename) -> import (In_channel.create filename)
    | None -> import stdin
    )

let list_command =
  Command.basic ~summary:"List stored entries."
    Command.Spec.(empty)
    (fun () -> print_endline "list")

let commands =
  Command.group ~summary:"Manage encrypted secrets."
    ["import", import_command; "list", list_command]

let () =
  Command.run ~version:"0.1.0" commands
