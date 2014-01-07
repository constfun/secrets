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

let commands =
  Command.group ~summary:"Manage encrypted secrets."
    ["init", init_command ]

let () =
  Unix.mkdir_p ~perm:0o700 secrets_rc_path;
  Command.run ~version:"0.1.0" commands
