open Core.Std
open Termbox

type t = {
  search_string : string;
}

let ctrl_C = '\x03'

let render_line n s =
  let w = Termbox.width () in
  let len = Int.min w ((String.length s) - 1) in
  for i = 0 to len do
    Termbox.set_cell_char i n (String.get s i)
  done;
  Termbox.present ()

let render ui =
  render_line 0 ("find: " ^ ui.search_string)

let rec loop ui =
  render ui;
  match Termbox.poll_event () with
  | Ascii c when c = ctrl_C -> ()
  | Ascii c when Char.is_print c || c = ' ' ->
    let s = ui.search_string ^ (Char.to_string c) in
    loop { search_string=s }
  | Ascii _
  | Key _ | Utf8 _ | Resize _ -> loop ui

let start () =
  ignore (Termbox.init ());
  loop { search_string="" };
  Termbox.shutdown ()
