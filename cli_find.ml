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
  results : qres list;
  summary : string array;
  summary_hl : hl array
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
  | Ascii c when Char.is_alphanum c || c = ' ' ->
      Some (text ^ (Char.to_string c))
  | Ascii c when c = '\x7F' (* backspace *) ->
      Some (String.drop_suffix text 1)
  | _ -> None

let results_cells r hl (x, y) _ =
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
  List.iteri results ~f:(fun i { summary; summary_hl; value=_ } ->
    lines.(i) <- summary;
    hl.(i) <- summary_hl
  );
  (results, lines, hl)

let slider_cells len sel (x, y) (w, _) =
  let ch = if y = sel && x = (w-1) && len > 0 then '>' else '\x00' in
  { ch; fg=Default; bg=Default }

type slider_action =
  | Update of int
  | Select of int

let slider_input e sel h =
  let move_sel_down = Update (Int.min (h - 1) (sel + 1)) in
  let move_sel_up = Update (Int.max 0 (sel - 1)) in
  match e with
  | Ascii c when c = '\x0D' (* ENTER *) -> Select sel
  | Key Arrow_down -> move_sel_down
  | Ascii c when c = '\x0A' -> move_sel_down
  | Key Arrow_up -> move_sel_up
  | Ascii c when c = '\x0B' -> move_sel_up
  | _ -> Update sel

let rec loop state =
  let winw = Termbox.width () in
  let winh = Termbox.height () in
  let sliderh = winh - 1 in
  let (inputx, inputy) as input_pos = (6, 0) in
  render_cells ~pos:(0, 0) ~size:(6, 1) ~f:(string_cells "find: ");
  render_cells ~pos:input_pos ~size:((winw-6), 1) ~f:(string_cells state.query);
  render_cells ~pos:(6, 1) ~size:((winw-6), (winh-1)) ~f:(results_cells state.summary state.summary_hl);
  render_cells ~pos:(0, 1) ~size:(5, sliderh) ~f:(slider_cells (List.length state.results) state.selection);
  Termbox.set_cursor (inputx + (String.length state.query)) inputy;
  Termbox.present ();
  let state = match Termbox.poll_event () with
    | Ascii c when c = '\x03' (* CTRL_C *) -> None
    | (Ascii _ | Key _ ) as e ->
        (match handle_input e state.query with | Some query ->
            let (results, summary, summary_hl) = search state.secrets query in
            Some { state with query; results; summary; summary_hl; selection=0 }
        | None ->
            (match slider_input e state.selection (Int.min (List.length state.results) sliderh) with
            | Update selection -> Some { state with selection }
            | Select selection ->
                Utils.pbcopy (List.nth_exn state.results selection).value;
                None
            )
        )
    | Utf8 _ | Resize _ -> Some state
  in
  match state with
  | Some s -> loop s
  | None -> ()

let run_loop state =
  ignore (Termbox.init ());
  (try loop state with
  | _ as e ->
      Termbox.shutdown ();
      raise e);
  Termbox.shutdown ()

let start secrets queryopt =
  match queryopt with
  | Some query -> (
      match search secrets query with
      | ([r], _, _) -> (* There is only one result, copy it to the clipboard without launching the find ui. *)
          print_endline (r.summary ^ " copied to your clipboard.");
          Utils.pbcopy r.value
      | (results, summary, summary_hl) -> run_loop { secrets; query; selection=0; results; summary; summary_hl } )
  | None -> run_loop { secrets; query=""; selection=(-1); results=[]; summary=[||]; summary_hl=[||] }
