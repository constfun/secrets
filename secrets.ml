open Core.Std
open Re2


type entry = { title : string; payload : (string * string) list } with sexp
type entry_with_password = { entry : entry; password : string }
type 'a db = 'a list with sexp

let key = "01234567891011121314151617181920"

let load filename =
  In_channel.read_all filename
  |> Nacl.Secretbox.of_string
  |> Nacl.Secretbox.box_open key
  |> Sexp.of_string
  |> db_of_sexp entry_of_sexp

let save db filename =
  let data = sexp_of_db sexp_of_entry db
  |> Sexp.to_string
  |> Nacl.Secretbox.box key
  |> Nacl.Secretbox.to_string in
  Out_channel.write_all filename ~data

let list filename =
  let db = load filename in
  List.iter db ~f:(fun e -> print_endline e.title)

let passfor filename query =
  let r = Regex.create_exn (String.concat_map query ~sep:".*?" ~f:String.of_char) in
  load filename
  |> List.filter ~f:(fun e -> Regex.matches r e.title)
  |> List.fold ~init:[] ~f:(fun db' entry ->
      let password_entry = List.find_map entry.payload ~f:(fun p -> match p with
        | ("password", password) -> Some { entry; password }
        | _ -> None
      ) in
      match password_entry with
      | Some e -> e :: db'
      | None -> db'
    )
  |> List.iter ~f:(fun e -> print_endline e.entry.title)

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
