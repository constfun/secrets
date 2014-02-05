open Core.Std
open Secrets
open Termbox

type t = {
  secrets : Secrets.t;
  search_string : string;
  search_results : string list
}

let ctrl_C = '\x03'
let backspace = '\x7F'

let render_line n s =
  let w = Termbox.width () in
  let slen = String.length s in
  for i = 0 to w do
    let c = if i < slen then String.get s i else '\x00' in
    Termbox.set_cell_char i n c
  done

let clear_line n = render_line n ""

let rec render_results res start stop =
  if start > stop then () else
    match res with
    | hd :: tail ->
        render_line start hd;
        render_results tail (start + 1) stop
    | [] ->
        clear_line start;
        render_results res (start + 1) stop

let render ui =
  let search_field = "find: " ^ ui.search_string in
  Termbox.set_cursor (String.length search_field) 0;
  render_line 0 search_field;
  render_results ui.search_results 1 (Termbox.height ());
  Termbox.present ()

let update_search ui search_string =
  let search_results = Secrets.search ui.secrets search_string in
  { ui with search_string; search_results }

let rec loop ui =
  render ui;
  match Termbox.poll_event () with
  | Ascii c when c = ctrl_C -> ()
  | Ascii c when Char.is_print c || c = ' ' ->
    let search_string = ui.search_string ^ (Char.to_string c) in
    loop (update_search ui search_string)
  | Ascii c when c = backspace ->
    let search_string = String.drop_suffix ui.search_string 1 in
    loop (update_search ui search_string)
  | Ascii _
  | Key _ | Utf8 _ | Resize _ -> loop ui

let start secrets =
  ignore (Termbox.init ());
  loop { secrets; search_string=""; search_results=[] };
  Termbox.shutdown ()
