open Core.Std
open Core_extended.Std

let secrets_rc_path = Filename.expand "~/.secrets"

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

let init_command =
  Command.basic ~summary:"Create a new secrets file."
    Command.Spec.(
      empty
      +> anon ("path" %: filename ~should_exist:false )
    )
    (fun path () ->
      let keypath = Filename.implode [secrets_rc_path; "key"] in
      let key = Key.create keypath in
      ignore (Secrets.create key path))

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

let commands =
  Command.group ~summary:"Manage encrypted secrets."
    ["init", init_command ]

let () =
  Unix.mkdir_p ~perm:0o700 secrets_rc_path;
  Command.run ~version:"0.1.0" commands
