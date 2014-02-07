open Core.Std
open Secrets
open Termbox


type cell = {
  fg : color;
  bg : color;
  ch : char
}
let blank_cell = { fg=Default; bg=Default; ch='\x00' }

type pos = int * int
type size = int * int
type conf = {
  pos : pos;
  size : pos;
  is_visible : bool;
}

module Control = struct
  module type S = sig
    type t
    val get_cell : t -> pos -> size -> cell
    val handle_event : t -> size -> event -> unit
  end
end

module type Control_instance = sig
  module C : Control.S
  val conf : conf
  val ctl : C.t
end

let make_control_instance (type a)
  (module C : Control.S with type t = a) ?(is_visible=true) ~pos ~size ctl
  =
  (module struct
    module C = C
    let conf = { pos; size; is_visible }
    let ctl = ctl
  end : Control_instance)


module Label = struct
  type t = {
    lines : string array ref;
    hl : hl array ref;
  }

  let create ?(hl=[||]) lines =
    { lines=(ref lines); hl=(ref hl) }

  let get_cell l (x, y) _ =
    let blank = ('\x00', Default) in
    let ch, fg = (
      if y >= (Array.length !(l.lines)) then blank else
        let text = !(l.lines).(y) in
        let tlen = String.length text in
        let fg = if y >= (Array.length !(l.hl)) then Default else (
          let hl = !(l.hl).(y) in
          if (Set.mem hl x) then Red else Default
        ) in
        if x < tlen then (String.get text x, fg) else blank
    ) in
    { ch; fg; bg=Default }

  let handle_event _ _ _ = ()
end

module Input  = struct
  type t = string ref

  let create () = ref ""

  let get_cell inp (x, y) _ =
    let tlen = String.length !(inp) in
    let ch = if x < tlen && y = 0 then String.get !(inp) x else '\x00' in
    { ch; fg=Default; bg=Default }

  let handle_event inp _ e =
    match e with
    | Ascii c when Char.is_print c || c = ' ' ->
        let text = !(inp) ^ (Char.to_string c) in
        inp := text
    | Ascii c when c = '\x7F' (* backspace *) ->
        let text = String.drop_suffix !(inp) 1 in
        inp := text
    | _ -> ()
end


module Slider = struct
  type t = int ref

  let create cur = ref cur

  let get_cell sl (x, y) (w, _) =
    if y = !sl && x = (w-1) then
      { ch='>'; fg=Default; bg=Default }
    else
      blank_cell

  let handle_event sl (_, h) e =
    match e with
    | Key Arrow_down ->
        sl := Int.min (h - 1) (!sl + 1)
    | Key Arrow_up ->
        sl := Int.max 0 (!sl -1)
    | _ -> ()
end

let render_controls ctls =
  List.iter ctls ~f:(fun (module I : Control_instance) ->
    let { pos=(x, y); size=(w, h); is_visible } = I.conf in
    for cy = 0 to h-1 do
      for cx = 0 to w-1 do
        let cell =
          if is_visible then I.C.get_cell I.ctl (cx, cy) (w, h)
          else blank_cell
        in
        Termbox.set_cell_char ~fg:cell.fg ~bg:cell.bg (x + cx) (y + cy) cell.ch
      done
    done
  );
  Termbox.present ()

let dispatch_event ctls e =
  List.iter ctls ~f:(fun (module I : Control_instance) ->
    I.C.handle_event I.ctl I.conf.size e
  )


type state = {
  secrets : Secrets.t;
  input_ctl : Input.t;
  results_ctl : Label.t;
  slider_ctl : Slider.t;
  controls : (module Control_instance) list;
  results: qres list
}


let rec loop state =
  render_controls state.controls;
  match Termbox.poll_event () with
  | Ascii c when c = '\x03' (* CTRL_C *) -> ()
  | (Ascii _ | Key _ | Utf8 _) as e ->
      dispatch_event state.controls e;
      loop state
  (*
      let results = (
        Input.handle_event state.input_ctl e;
        match !(state.input_ctl.conf.text) with
        | "" -> state.results
        | s ->
          let results = Secrets.search state.secrets query in
          (match List.length results with
            | 0 ->
                Label.update state.results_ctl [|"-no results-"|];
                Slider.set_visibility state.slider_ctl false
            | rlen ->
              let lines = Array.create rlen "" in
              let hl = Array.create rlen (Set.empty Int.comparator) in
              List.iteri results ~f:(fun i { summary; summary_hl; _ } ->
                lines.(i) <- summary;
                hl.(i) <- summary_hl);
              Label.update state.results_ctl lines ~hl;
              Slider.set_visibility state.slider_ctl true
          );
          results
      ) in
      Slider.handle_event state.slider_ctl e;
      loop { state with results }
  *)
  | Resize _ -> loop state


let start secrets =
  ignore (Termbox.init ());
  let winw = Termbox.width () in
  let winh = Termbox.height () in
  let results_ctl = Label.create  [|"-start typing to fuzzy search your secrets-"|] in
  let input_ctl = Input.create ~on_change:(fun text ->

  ) in
  let slider_ctl = Slider.create 0 in
  let controls = [
    make_control_instance (module Label) ~pos:(0, 0) ~size:(6, 1) (Label.create [|"find: "|]);
    make_control_instance (module Input) ~pos:(6, 0) ~size:((winw - 6), 1) input_ctl;
    make_control_instance (module Label) ~pos:(6, 1) ~size:((winw - 6), (winh - 1)) results_ctl;
    make_control_instance (module Slider) ~pos:(0, 1) ~size:(5, (winh - 1)) slider_ctl;
  ] in
  loop { secrets; input_ctl; results_ctl; slider_ctl; controls; results=[] };
  Termbox.shutdown ()
