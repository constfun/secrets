open Core.Std

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
    | Some(filename) -> Secrets.import (In_channel.create filename) target_filename
    | None -> Secrets.import stdin target_filename
    )

let list_command =
  Command.basic ~summary:"List stored entries."
    Command.Spec.(
      empty
      ++ secrets_file_flag ~should_exist:true
    )
    (fun filename () -> Secrets.list filename)

let passfor_command =
  Command.basic ~summary:"Fuzzy search entries that contain a password."
    Command.Spec.(
      empty
      +> anon ("query" %: string)
      ++ secrets_file_flag ~should_exist:true
    )
    (fun query filename () -> Secrets.passfor filename query )

let randpass_command =
  Command.basic ~summary:"Fuzzy search entries that contain a password."
    Command.Spec.(
      empty
      ++ secrets_file_flag ~should_exist:true
    )
    (fun filename () -> Secrets.randpass filename )

let newkey_command =
  Command.basic ~summary:"Generate a new key file."
    Command.Spec.(empty)
    (fun () -> Scrypt.hello )

let commands =
  Command.group ~summary:"Manage encrypted secrets."
    ["import", import_command; "list", list_command; "passfor", passfor_command; "randpass", randpass_command; "newkey", newkey_command ]

let () =
  Command.run ~version:"0.1.0" commands
