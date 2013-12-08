open Core.Std


let list () =
  print_string "Listing\n"

let import file =
  printf "Importing from file\n"

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
