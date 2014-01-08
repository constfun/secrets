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

let () =
  Unix.mkdir_p ~perm:0o700 secrets_rc_path;
  let key_path = Filename.implode [secrets_rc_path; "key"] in

  let open Command in
  let init_cmd = basic ~summary:"Create a new secrets file."
    Spec.(empty +> anon ("path" %: filename ~should_exist:false))
    (fun path () ->
      let key = Crypto.create key_path in
      Crypto.with_file path ~key:key ~f:(fun _ ->
        Secrets.to_string (Secrets.create ())
      ))
  in
  run ~version:"0.1.0"
    (group ~summary:"Manage encrypted secrets." [
        "init", init_cmd
    ])
