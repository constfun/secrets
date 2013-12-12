open Core.Std
open Re2

type entry = { title : string; payload : (string * string) list; } with sexp
type 'a db = 'a list with sexp

let read filename =
  let s = In_channel.read_all filename in
  Nacl.Secretbox.of_list (String.to_list s)

let write target_filename encrypted_data =
  Out_channel.with_file target_filename ~f:(fun ic ->
    List.iter (Nacl.Secretbox.to_list encrypted_data) (Out_channel.output_char ic)
  )

let encode db =
  sexp_of_db sexp_of_entry db

let encrypt data =
  let key = "01234567891011121314151617181920" in
  Nacl.Secretbox.box key data

let decrypt encrypted =
  let key = "01234567891011121314151617181920" in
  Nacl.Secretbox.box_open key encrypted

let decode encoded =
  db_of_sexp entry_of_sexp encoded

let load filename =
  read filename |> decrypt |> Sexp.of_string |> decode

let save db target_filename =
  encode db |> Sexp.to_string |> encrypt |> write target_filename

let list filename =
  let db = load filename in
  List.iter db ~f:(fun e -> print_endline e.title)

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
    Command.Spec.(
      empty
      ++ secrets_file_flag ~should_exist:true
    )
    (fun filename () -> list filename)

let commands =
  Command.group ~summary:"Manage encrypted secrets."
    ["import", import_command; "list", list_command]

let () =
  Command.run ~version:"0.1.0" commands
