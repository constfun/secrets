open Core.Std
open Re2

type entry = { title : string; payload : (string * string) list; } with sexp
type 'a db = 'a list with sexp

let list () =
  print_string "Listing\n"

let save db target_filename =
  let encoded = Sexp.to_string (sexp_of_db sexp_of_entry db) in
  let encrypted = Crypto.encrypt encoded in
  Out_channel.with_file target_filename ~f:(fun ic ->
    List.iter (Crypto.to_char_list encrypted) (Out_channel.output_char ic)
  )

let import file target_filename =
  let r = Regex.create_exn "(^[^[].*)[\\s](.*)$" in
  let db = In_channel.fold_lines file ~init:[] ~f:(fun db l ->
    match Regex.find_submatches r l with
    | Error _ -> db
    | Ok submatches ->
      let title = match submatches.(1) with Some(t) -> String.strip t | None -> assert false in
      let password = match submatches.(2) with Some(p) -> String.strip p | None -> assert false in
      let entry = { title; payload=[("password", password)] } in
      entry :: db
  ) in
  save db target_filename

let filename ~should_exist =
  Command.Spec.Arg_type.create
    (fun filename -> match (Sys.is_file filename), should_exist with
    | `Yes, true | `No, false -> filename
    | `Yes, false ->
      eprintf "'%s' file already exists.\n%!" filename;
      exit 1
    | `No, true | `Unknown, _ ->
      eprintf "'%s' file not found.\n%!" filename;
      exit 1
    )

let secrets_file_flag ~should_exist =
  Command.Spec.(
    empty
    +> flag "-secrets-file" (required (filename ~should_exist)) ~doc:"Location of the secrets file."
  )

let import_command =
  Command.basic ~summary:"Import entries from old format."
    Command.Spec.(
      empty
      +> anon (maybe ("filename" %: filename ~should_exist:true ))
      ++ secrets_file_flag ~should_exist:false
    )
    (fun source_filename target_filename () -> match source_filename with
    | Some(filename) -> import (In_channel.create filename) target_filename
    | None -> import stdin target_filename
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
