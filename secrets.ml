open Core.Std
open Re2


type entry = { title : string; payload : (string * string) list } with sexp
type entry_with_password = { entry : entry; password : string }
type 'a db = 'a list with sexp

let key = "01234567891011121314151617181920"

let create a b = ()

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

let rec rangeinput s e =
  printf "[%i-%i] %!" s e;
  let n = Scanf.scanf "%i\n" Fn.id in
  if n >= s && n <= e then n
  else rangeinput s e

let choose choices = match choices with
  | [(_, c)] -> Some c
  | [] -> None
  | _ :: _ ->
    Out_channel.output_string stdout "Which one?\n";
    List.iteri choices ~f:(fun i (s, _) -> Out_channel.output_string stdout (sprintf "%i: %s\n" (i + 1) s));
    Out_channel.flush stdout;
    let ci = (rangeinput 1 (List.length choices)) - 1 in
    let (_, c) = List.nth_exn choices ci in
    Some c

let pbcopy s =
  let po = Unix.open_process_out "pbcopy" in
  Out_channel.output_string po s;
  ignore (Unix.close_process_out po)

let randstring =
  Random.self_init ();
  String.init ~f:(fun i -> Char.of_int_exn (33 + Random.int 94))

let randpass ?(len=20) filename =
  let db = load filename in
  printf "Entry title: %!";
  match In_channel.input_line stdin with
  | Some title ->
      let p = (randstring len) in
      save ( {title; payload=[ ("password", p) ]} :: db ) filename;
      pbcopy p
  | None -> Out_channel.output_string stderr "Canceled. A title is required.\n"

let passfor filename query =
  let r = Regex.create_exn (String.concat_map query ~sep:".*?" ~f:String.of_char) in
  let entry =
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
    |> List.map ~f:(fun e -> (e.entry.title, e))
    |> choose in
  match entry with
  | None -> print_endline "Nothing found."
  | Some e -> pbcopy e.password

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
