open Core.Std
open Secrets
open Termbox

type cell = {
  fg : color;
  bg : color;
  ch : char
}

type state = {
  secrets : Secrets.t;
  query : string;
  selection : int;
  results : string array;
  results_hl : hl array
}

let render_cells ~pos:(x, y) ~size:(w, h) ~f =
  for cy = 0 to h-1 do
    for cx = 0 to w-1 do
      let cell = f (cx, cy) (w, h) in
      Termbox.set_cell_char ~fg:cell.fg ~bg:cell.bg (x + cx) (y + cy) cell.ch
    done
  done

let string_cells s (x, y) _ =
  let len = String.length s in
  let ch = if x < len && y = 0 then String.get s x else '\x00' in
  { ch; fg=Default; bg=Default }

let handle_input e text =
  match e with
  | Ascii c when Char.is_print c || c = ' ' ->
      text ^ (Char.to_string c)
  | Ascii c when c = '\x7F' (* backspace *) ->
      String.drop_suffix text 1
  | _ -> text

let results_cells r hl (x, y) (w, h) =
  let blank = ('\x00', Default) in
  let ch, fg = (
    if y >= (Array.length r) then blank else
      let text = r.(y) in
      let tlen = String.length text in
      let fg = if y >= (Array.length hl) then Default else (
        let hl = hl.(y) in
        if (Set.mem hl x) then Red else Default
      ) in
      if x < tlen then (String.get text x, fg) else blank
  ) in
  { ch; fg; bg=Default }

let search secrets query =
  let results = Secrets.search secrets query in
  let len = List.length results in
  let lines = Array.create ~len "" in
  let hl = Array.create ~len (Set.empty ~comparator:Int.comparator) in
  List.iteri results ~f:(fun i { summary; summary_hl; _ } ->
    lines.(i) <- summary;
  hl.(i) <- summary_hl);
  (lines, hl)

let slider_cells r sel (x, y) (w, h) =
  let rlen = Array.length r in
  let ch = if y = sel && x = (w-1) && rlen > 0 then '>' else '\x00' in
  { ch; fg=Default; bg=Default }

let slider_input e sel h =
  match e with
  | Key Arrow_down -> Int.min (h - 1) (sel + 1)
  | Key Arrow_up -> Int.max 0 (sel - 1)
  | _ -> sel

let rec loop state =
  let winw = Termbox.width () in
  let winh = Termbox.height () in
  let sliderh = winh - 1 in
  render_cells ~pos:(0, 0) ~size:(6, 1) ~f:(string_cells "find: ");
  render_cells ~pos:(6, 0) ~size:((winw-6), 1) ~f:(string_cells state.query);
  render_cells ~pos:(6, 1) ~size:((winw-6), (winh-1)) ~f:(results_cells state.results state.results_hl);
  render_cells ~pos:(0, 1) ~size:(5, sliderh) ~f:(slider_cells state.results state.selection);
  Termbox.present ();
  let state = match Termbox.poll_event () with
    | Ascii c when c = '\x03' (* CTRL_C *) -> None
    | (Ascii _ | Key _ | Utf8 _) as e ->
        let query = handle_input e state.query in
        let (results, results_hl) = search state.secrets query in
        let selection = slider_input e state.selection (Int.min (Array.length results) sliderh) in
        Some { state with query; results; results_hl; selection }
    | Resize _ -> Some state
  in
  match state with
  | Some s -> loop s
  | None -> ()

let start secrets =
  ignore (Termbox.init ());
  loop { secrets; query=""; selection=(-1); results=[||]; results_hl=[||] };
  Termbox.shutdown ()
