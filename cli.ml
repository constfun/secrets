open Core
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
      | Sec -> Secrets.of_string (In_channel.input_all In_channel.stdin)
      | Tsv ->
          let payload_keys =
            match In_channel.input_line In_channel.stdin with
            | Some l ->
                (match String.split l ~on:'\t' with
                | _ :: payload_keys -> payload_keys
                | [] -> raise Invalid_format )
            | None -> raise Invalid_format in
          In_channel.fold_lines In_channel.stdin ~init:Secrets.empty ~f:(fun sec l ->
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

(* let add = with_secrets_file ~f:(fun sec -> *)
(*     let new_entries = edit_and_parse () in *)
(*     Secrets.append sec new_entries *)
(*   ) *)
open Incremental_lib


module AddView = struct
  module Inc = Incremental.Make ()
  open Inc

  type t = {
    title: string Var.t;
    (* focused: ('a Var.t -> Notty *)
  }

  let create () = {
    title = Var.create ""
  }

  (* let img = I.uchars A.(fg red) (Array.init 80 (fun _ -> Uchar.of_int 0x2593)) in *)
  (* Notty_unix.output_image img *)

  (* let edit_str_mutation var = function *)
  (*   | `Key (`ASCII c, []) -> Var.set var ((Var.value var) ^ (Char.to_string c)) *)
  (*   | _ -> () *)

  type ret = Exit | Continue

  let process_input t term =
    let open Notty_unix in
    let skip () = Term.refresh term; Continue in
    match Term.event term with
      | `Key (`ASCII 'q', `Ctrl :: []) -> Exit
      | `Key (`ASCII c, []) -> Var.set t.title ((Var.value t.title) ^ (Char.to_string c)); Continue
      | _ -> skip ()

  let init t term =
    let open Notty in
    let open Notty_unix in
    let view = map (Var.watch t.title) (fun titl ->
      I.string A.(fg red) titl
    ) in
    let view_o = observe view in
    let rec loop () =
      stabilize ();
      Term.image term (Observer.value_exn view_o);
      match process_input t term with
      | Exit -> ()
      | Continue -> loop ()
    in
    loop ()
end

let cassowary () =
  let open Cassowary in
  let open Ctypes in
  (* let printr = Result.(function *)
  (*   | Ok -> print_endline "Ok." *)
  (*   | Failed -> print_endline "Failed." *)
  (*   | Unsatisfied -> print_endline "Unsatisfied." *)
  (*   | Unbound -> print_endline "Unbound." *)
  (* ) in *)
  let s = newsolver null null in
  let xl = newvariable s in
  (* let xm = newvariable s in *)
  (* let xr = newvariable s in *)
  let c1 = newconstraint s Strength.Required in
  ignore (addterm c1 xl 1.0);
  ignore (setrelation c1 Relation.Greatequal);
  ignore (add c1);
  dumpsolver s;
  let c2 = newconstraint s Strength.Required in
  ignore (addterm c2 xl 1.0);
  ignore (setrelation c2 Relation.Equal);
  ignore (add c2);
  dumpsolver s

let yoga () =
  let open Ctypes in
  let open Yoga in
  let node = node_new () in
  node_style_set_width node 42.0;
  let width_yg_val = node_style_get_width node in
  let width = getf width_yg_val yg_val_value in
  print_endline ("Node width: " ^ (Float.to_string width));
  node_print node Style


module App (State : Incremental.S)  = struct
  open Tsdl

  type t = {
    win : Sdl.window;
    ctx : Cairo.context;
    width : float;
    height : float;
  }

  let log = Sdl.log

  let context_from_window_exn win =
    let w, h =  Sdl.get_window_size win in
    let screen_surface = Rresult.R.get_ok (Sdl.get_window_surface win) in
    let pixels = Sdl.get_surface_pixels screen_surface Bigarray.int8_unsigned in
    let cairo_surface = Cairo.Image.(create_for_data8 pixels ARGB32 w h) in
    (Cairo.create cairo_surface, float w, float h)

  let create ~title ~width ~height =
    let open State in
    let open Result.Monad_infix in
    let window_res =
      Sdl.init Sdl.Init.video >>= fun _ ->
      Sdl.create_window title ~w:width ~h:height Sdl.Window.(opengl + shown + resizable + allow_highdpi)
    in
    match window_res with
    | Error (`Msg e) -> log "Error initializing SDL: %s" e; exit 1
    | Ok win ->
      let (ctx, width, height) = context_from_window_exn win in
      { win; ctx; width; height; }


  let loop t draw =
    let outofloop_draw () =
      let (ctx, width, height) = context_from_window_exn t.win in
      draw ctx width height;
      match Sdl.update_window_surface t.win with
      | Error (`Msg e) -> log "Update window surface failed: %s" e; exit 1
      | Ok () -> ();
      Cairo.Surface.finish(Cairo.get_target ctx);
    in
    Patch.listen_for_resize_event outofloop_draw;
    let start () =
      let ev = Sdl.Event.create () in
      let rec event_loop t =
        let ev_res = Sdl.wait_event (Some ev) in
        match ev_res with
        | Error (`Msg e) -> log "Event loop error: %s" e; exit 1
        | Ok () ->
            match Sdl.Event.(enum (get ev typ)) with
            | `Window_event -> (
                match Sdl.Event.(window_event_enum (get ev window_event_id)) with
                | `Resized ->
                  (* We need to get a new context when the size of the window changes. *)
                  Cairo.Surface.finish(Cairo.get_target t.ctx);
                  let (ctx, width, height) = context_from_window_exn t.win in
                  render {t with ctx; width; height}
                | _ -> event_loop t
            )
            | `Key_down ->
                let kc = Sdl.Event.(get ev keyboard_keycode) in
                print_string (Sdl.get_key_name kc);
                Pervasives.flush_all ();
                event_loop t
            | `Quit -> ()
            | _ -> event_loop t
      and render t =
        draw t.ctx t.width t.height;
        match Sdl.update_window_surface t.win with
        | Error (`Msg e) -> log "Update window surface failed: %s" e; exit 1
        | Ok () -> event_loop t
      in
      Sdl.start_text_input ();
      render t
    in
    start ();
    Sdl.destroy_window t.win;
    Sdl.quit ()
end

let cairo () =
  let module State = Incremental.Make () in
  let module Esns = App(State) in
  let app = Esns.create ~title:"esns" ~width:570 ~height:415 in
  ignore app
  (* let dirty = Esns.is_dirty app in *)
  (* let size = Esns.size app in *)
  (* let draw_state = State.map size ~f:(fun (w, h) -> *)
  (*   (float w, float h, if w > h then (1., 0., 0.) else (0., 0., 1.)) *)
  (* ) in *)
  (* let draw_state_observer = State.observe draw_state in *)
  (* let draw () = *)
  (*   let state = State.Observer.value_exn draw_state_observer in *)
  (*   (1* Esns.draw app state *1) *)
  (* in *)
  (* Esns.loop app draw *)

let notty () =
  let term = Notty_unix.Term.create () in
  let view = AddView.create () in
  AddView.init view term

let draw ctx width height =
  let open Cairo in
  let pi2 = 8. *. Float.atan 1. in
  clear ();
  set_source_rgba ctx (180. /. 255.) (115. /. 255.) 1.0 1.;
  rectangle ctx 0. 0. width height;
  fill ctx;
  let r = 0.25 *. width in
  set_source_rgba ctx 0. 1. 0. 0.5;
  arc ctx (0.5 *. width) (0.35 *. height) r 0. pi2;
  fill ctx;
  set_source_rgba ctx 1. 0. 0. 0.5;
  arc ctx (0.35 *. width) (0.65 *. height) r 0. pi2;
  fill ctx;
  set_source_rgba ctx 0. 0. 1. 0.5;
  arc ctx (0.65 *. width) (0.65 *. height) r 0. pi2;
  fill ctx


let tsdl () =
  let module State = Incremental.Make () in
  let module Esns = App(State) in
  let app = Esns.create ~title:"esns" ~width:570 ~height:415 in
  Esns.loop app draw


let add = with_secrets_file ~f:(fun sec ->
    ignore(tsdl ());
    sec
  )

let edit = with_secrets_file ~f:(fun sec ->
    edit_and_parse ~init_contents:(Secrets.to_string sec) ()
  )

let rec rand_char ~alphanum =
  let c = Char.of_int_exn (33 + Random.int 94) in
  if not alphanum then c else
  match c with
  | c when Char.is_alphanum c -> c
  | _ -> rand_char ~alphanum


let rand_str ~len ~alphanum =
  Random.self_init ();
  String.init len ~f:(fun _ -> rand_char ~alphanum)

let rand ?(field="password") ?(len=20) ~alphanum ~title = with_secrets_file ~f:(fun sec ->
    let rand_val = rand_str ~len ~alphanum in
    let payload = [(field, rand_val)] in
    let e = Entry.create title payload in
    Utils.pbcopy rand_val;
    Secrets.add sec e
  )

let find ~query = with_secrets_file ~f:(fun sec ->
  Cli_find.start sec query;
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
  let rand_cmd = basic ~summary:"Add a new secret with a random password."
    Spec.(
      empty
      +> flag "-F" (optional_with_default "password" string) ~doc:"field-name Name to use for the random field. Default: password"
      +> flag "-l" (optional_with_default 20 int) ~doc:"length Length of the random value. Default: 20"
      +> flag "-s" no_arg ~doc:" Create a simple, alpha-numeric value. Default: false"
      +> anon ("title" %: string)
      )
    (fun field len alphanum title () ->
      match title with
      | "" ->
          eprintf "Title cannot be empty.\n";
          exit 1
      | title ->
          with_defaults ~f:(rand ~field ~len ~alphanum ~title))
  in
  let edit_cmd = basic ~summary:"Edit all secrets using $EDITOR."
    Spec.empty
    (fun () -> with_defaults ~f:edit)
  in
  let find_cmd = basic ~summary:"Start fuzzy search."
    Spec.(
      empty
      +> anon (maybe ("query" %: string))
    )
    (fun query () -> with_defaults ~f:(find ~query))
  in
  run ~version:"0.1.0"
    (group ~summary:"Manage encrypted secrets." [
      "init", init_cmd;
      "add", add_cmd;
      "rand", rand_cmd;
      "edit", edit_cmd;
      "find", find_cmd;
      "import", import_cmd;
      "export", export_cmd;
    ])
