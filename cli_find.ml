open Core
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
  | Ascii c when Char.is_alphanum c || Char.equal c ' ' ->
      Some (text ^ (Char.to_string c))
  | Ascii c when Char.equal c '\x7F' (* backspace *) ->
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
  let hl = Array.create ~len (Set.Using_comparator.empty ~comparator:Int.comparator) in
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
  | Ascii c when Char.equal c '\x0D' (* ENTER *) -> Select sel
  | Key Arrow_down -> move_sel_down
  | Ascii c when Char.equal c '\x0A' -> move_sel_down
  | Key Arrow_up -> move_sel_up
  | Ascii c when Char.equal c '\x0B' -> move_sel_up
  | _ -> Update sel

let rec loop state found =
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
  match Termbox.poll_event () with
    | Ascii c when Char.equal c '\x03' (* CTRL_C *) -> found None
    | (Ascii _ | Key _ ) as e ->
        (match handle_input e state.query with | Some query ->
            let (results, summary, summary_hl) = search state.secrets query in
            loop { state with query; results; summary; summary_hl; selection=0 } found 
        | None ->
            (match slider_input e state.selection (Int.min (List.length state.results) sliderh) with
            | Update selection -> loop { state with selection } found
            | Select selection -> found (List.nth state.results selection)
            )
        )
    | Utf8 _ | Resize _ -> loop state found

let rec full_screen_qr_code qr_matrix =
  let open Termbox in
  hide_cursor ();
  set_clear_attributes Termbox.Black Termbox.White;
  clear ();
  let w = Qrc.Matrix.w qr_matrix in
  let offset_x, offset_y = (Termbox.width () - w * 2) / 2, (Termbox.height () - w) / 2 in
  let cx = ref 0 in
  while !cx < w do (
    let cy = ref 0 in
    while !cy < w do
      let ch = Int32.of_int_exn (if Qrc.Matrix.get qr_matrix ~x:!cx ~y:!cy then 9608 else 32) in
      let set_at_x x = set_cell_utf8 ~fg:Termbox.Black ~bg:Termbox.White (offset_x + x) (offset_y + !cy) ch in
      set_at_x (!cx * 2);
      set_at_x (!cx * 2 + 1);
      cy := !cy + 1
    done;
    cx := !cx + 1)
  done;
  present ();
  match poll_event () with
  | Resize _ -> full_screen_qr_code qr_matrix
  | _ -> () 


let run_loop state found =
  ignore (Termbox.init ());
  let res = loop state found in
  Termbox.shutdown ();
  res
  
let start secrets qr queryopt =
  let found = function
    | Some (res : qres) ->
       if qr then
         (match Qrc.encode res.value with
          | Some m -> full_screen_qr_code m          
          | None -> print_endline "Too much data for QR code!")
       else (
         print_endline (res.summary ^ " copied to your clipboard.");
         Utils.pbcopy res.value)
    | None -> ()
  in
  match queryopt with
  | Some query -> (
    match search secrets query with
    (* There is only one result, use it without launching the find ui. *)
    | ([res], _, _) -> (ignore (Termbox.init ()); found (Some res); Termbox.shutdown ())
    | (results, summary, summary_hl) ->
       run_loop { secrets; query; selection=0; results; summary; summary_hl } found)
  | None -> run_loop { secrets; query=""; selection=(-1); results=[]; summary=[||]; summary_hl=[||] } found
