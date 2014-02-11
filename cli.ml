open Core.Std
open Core_extended.Std
open Secrets
open Entry
open Termbox

type io_format =
  | Tsv
  | Sec

exception Invalid_format

let rc_path = Filename.expand "~/.secrets"
let rc_key_path = Filename.implode [rc_path; "key"]
let rc_sec_path = Filename.implode [rc_path; "default"]

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

let with_secrets_file ~key_path ~sec_path ~f =
  let key = Crypto.create key_path in
  Crypto.with_file sec_path ~key:key ~f:(fun s ->
    let sec = match String.length s with
    | 0 -> Secrets.empty
    | _ -> Secrets.of_string s in
    Secrets.to_string (f sec)
  )

let init key_path sec_path =
  Unix.mkdir_p ~perm:0o700 rc_path;
  with_secrets_file ~key_path ~sec_path ~f:Fn.id;
  if not (Sys.file_exists_exn ~follow_symlinks:false rc_sec_path)
  then Unix.symlink ~src:(Filename.realpath sec_path) ~dst:rc_sec_path

let import ~fmt = with_secrets_file ~f:(fun _ ->
  try (match fmt with
      | Sec -> Secrets.of_string (In_channel.input_all stdin)
      | Tsv ->
          let payload_keys =
            match In_channel.input_line stdin with
            | Some l ->
                (match String.split l ~on:'\t' with
                | _ :: payload_keys -> payload_keys
                | [] -> raise Invalid_format )
            | None -> raise Invalid_format in
          In_channel.fold_lines stdin ~init:Secrets.empty ~f:(fun sec l ->
            match String.split l ~on:'\t' with
            | title :: payload_values ->
                let payload = List.zip_exn payload_keys payload_values in
                let entry = Entry.create title payload in
                Secrets.add sec entry
            | [] -> raise Invalid_format))
  with Invalid_format -> eprintf "Invalid format."; exit 1
  )

let export = with_secrets_file ~f:(fun sec ->
  Out_channel.output_string stdout (Secrets.to_string sec);
  sec
  )

let rec edit_and_parse ?init_contents () =
  let write_fn = match init_contents with
  | Some init_contents -> (fun oc -> Out_channel.output_string oc init_contents)
  | None -> ignore in
  Filename.with_open_temp_file  "edit" ".sec" ~write:write_fn ~in_dir:rc_path ~f:(fun fname ->
    let editor = match Sys.getenv "EDITOR" with
    | Some e -> e
    | None -> "vim" in
    ignore (Unix.system (sprintf "%s '%s'" editor fname));
    let fcontents = In_channel.read_all fname in
    try Secrets.of_string fcontents with
    | Secrets_parser.Error ->
        let rec ask = (fun () ->
          printf "Parsing error. (e)dit or (a)bort? %!";
          match Scanf.scanf "%c\n" Fn.id with
          | 'a' -> Secrets.empty
          | 'e' -> edit_and_parse ~init_contents:fcontents ()
          | _ -> ask ()
        ) in
        ask ()
    )

let add = with_secrets_file ~f:(fun sec ->
    let new_entries = edit_and_parse () in
    Secrets.append sec new_entries
  )

let edit = with_secrets_file ~f:(fun sec ->
    edit_and_parse ~init_contents:(Secrets.to_string sec) ()
  )

let find = with_secrets_file ~f:(fun sec ->
  Cli_find.start sec;
  sec
  )

let with_defaults ~f =
  let sec_path = Filename.realpath rc_sec_path in
  f ~key_path:rc_key_path ~sec_path

let io_format =
  let map = String.Map.of_alist_exn ["tsv", Tsv; "sec", Sec;] in
  Command.Spec.Arg_type.of_map map

let () =
  let open Command in
  let init_cmd = basic ~summary:"Create a new secrets file."
    Spec.(empty +> anon ("path" %: filename ~should_exist:false))
    (fun sec_path () -> init rc_key_path sec_path)
  in
  let import_cmd = basic ~summary:"Import secrets from stdin."
    Spec.(empty +> flag "-fmt" (optional_with_default Sec io_format) ~doc:"<tsv,sec> Input format to expect.")
    (fun fmt () -> with_defaults ~f:(import ~fmt))
  in
  let export_cmd = basic ~summary:"Export secrets to stdin."
    Spec.empty
    (fun () -> with_defaults ~f:export)
  in
  let add_cmd = basic ~summary:"Add a new secret using $EDITOR."
    Spec.empty
    (fun () -> with_defaults ~f:add)
  in
  let edit_cmd = basic ~summary:"Edit all secrets using $EDITOR."
    Spec.empty
    (fun () -> with_defaults ~f:edit)
  in
  let find_cmd = basic ~summary:"Start fuzzy search."
    Spec.empty
    (fun () -> with_defaults ~f:find)
  in
  run ~version:"0.1.0"
    (group ~summary:"Manage encrypted secrets." [
      "init", init_cmd;
      "add", add_cmd;
      "edit", edit_cmd;
      "find", find_cmd;
      "import", import_cmd;
      "export", export_cmd;
    ])
