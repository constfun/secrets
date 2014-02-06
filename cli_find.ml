open Core.Std
open Secrets
open Termbox


(*
let search_box_indx = 0

let ctrl_C = '\x03'
let backspace = '\x7F'

let rec render_results ui res start stop =
  if start > stop then () else
    match res with
    | r :: tail ->
        render_line ~hl:r.summary_hl start r.summary;
        render_results ui tail (start + 1) stop
    | [] ->
        clear_line start;
        render_results ui res (start + 1) stop

let render ui =
  let sb = ui.controls.(search_box_indx) in
  let sb_rect = Label.get_rect sb in
  let sb_text = Label.get_text sb in
  Termbox.set_cursor (sb_rect.x + (String.length sb_text)) sb_rect.y;
  render_results ui ui.search_results 1 (Termbox.height ());
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
  *)

type cell = {
  fg : color;
  bg : color;
  ch : char
}


module type Control = sig
  type t
  val get_cell : t -> int -> int -> cell
  val get_size : t -> int * int
end


module type Control_instance = sig
  module C : Control
  val pos : int * int
  val ctl : C.t
end


let make_instance (type a)
  (module C : Control with type t = a) pos ctl
  =
  (module struct
    module C = C
    let pos = pos
    let ctl = ctl
  end : Control_instance)


module Label : sig
  include Control
  val create : ?hl : hl -> int * int -> string -> t
end = struct
  type t = {
    size : int * int;
    text : string;
    hl : hl;
  }

  let nohl = Set.empty Int.comparator

  let create ?(hl=nohl) size text = { size; text; hl }
  let get_size l = l.size
  let get_cell l x y =
    let tlen = String.length l.text in
    let ch = if x < tlen then String.get l.text x else '\x00' in
    let fg = if (Set.mem l.hl x) then Red else Default in
    { ch; fg; bg=Default }
end


let render_control x y (module I : Control_instance) =
  let (w, h) = I.C.get_size I.ctl in
  for cy = y to h-1 do
    for cx = x to w-1 do
      let cell = I.C.get_cell I.ctl cx cy in
      Termbox.set_cell_char ~fg:cell.fg ~bg:cell.bg cx cy cell.ch
    done
  done


let render_controls ctls =
  List.iter ctls ~f:(fun c ->
    render_control 0 0 c
  );
  Termbox.present ()


type state = {
  secrets : Secrets.t;
  controls : (module Control_instance) list;
  search_results : qres list
}


let rec loop state =
  render_controls state.controls;
  match Termbox.poll_event () with
  | Key _ -> ()
  | _ -> loop state


let start secrets =
  ignore (Termbox.init ());
  let winw = Termbox.width () in
  let winh = Termbox.height () in
  let controls = [
    make_instance (module Label) (0, 0) (Label.create (6, 1) "find: ")
  ] in
  loop { secrets; controls; search_results=[] };
  Termbox.shutdown ()
