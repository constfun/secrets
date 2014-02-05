open Core.Std
open Termbox

type t = {
  search_string : string;
}

let ctrl_C = '\x03'
let backspace = '\x7F'

let render_line n s =
  let w = Termbox.width () in
  let slen = String.length s in
  for i = 0 to w do
    let c = if i < slen then String.get s i else '\x00' in
    Termbox.set_cell_char i n c
  done;
  Termbox.present ()

let render ui =
  let search_field = "find: " ^ ui.search_string in
  Termbox.set_cursor (String.length search_field) 0;
  render_line 0 search_field

let rec loop ui =
  render ui;
  match Termbox.poll_event () with
  | Ascii c when c = ctrl_C -> ()
  | Ascii c when Char.is_print c || c = ' ' ->
    let s = ui.search_string ^ (Char.to_string c) in
    loop { search_string=s }
  | Ascii c when c = backspace ->
    let s = String.drop_suffix ui.search_string 1 in
    loop { search_string=s }
  | Ascii _
  | Key _ | Utf8 _ | Resize _ -> loop ui

let start () =
  ignore (Termbox.init ());
  loop { search_string="" };
  Termbox.shutdown ()
