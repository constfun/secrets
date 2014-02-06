open Core.Std
open Secrets
open Termbox


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


let make_control_instance (type a)
  (module C : Control with type t = a) pos ctl
  =
  (module struct
    module C = C
    let pos = pos
    let ctl = ctl
  end : Control_instance)


module Label : sig
  include Control
  val create : ?hl : hl array -> int * int -> string array -> t
  val update : ?hl : hl array -> t -> string array -> unit
end = struct
  type t = {
    size : int * int;
    lines : string array ref;
    hl : hl array ref;
  }

  let create ?(hl=[||]) size lines = { size; lines=(ref lines); hl=(ref hl) }

  let update ?(hl=[||]) l lines =
    l.lines := lines;
    l.hl := hl

  let get_size l = l.size

  let get_cell l x y =
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
end


module Input : sig
  include Control
  val create : int * int -> t
  val handle_event : t -> event -> string option
end = struct
  type t = {
    size : int * int;
    text : string ref;
  }

  let create size = { size; text=(ref "") }
  let get_size inp = inp.size

  let get_cell inp x y =
    let tlen = String.length !(inp.text) in
    let ch = if x < tlen then String.get !(inp.text) x else '\x00' in
    { ch; fg=Default; bg=Default }

  let handle_event inp e =
    match e with
    | Ascii c when Char.is_print c || c = ' ' ->
        let text = !(inp.text) ^ (Char.to_string c) in
        inp.text := text;
        Some text
    | Ascii c when c = '\x7F' (* backspace *) ->
        let text = String.drop_suffix !(inp.text) 1 in
        inp.text := text;
        Some text
    | _ -> None
end


let render_controls ctls =
  List.iter ctls ~f:(fun (module I : Control_instance) ->
    let (x, y) = I.pos in
    let (w, h) = I.C.get_size I.ctl in
    for cy = 0 to h-1 do
      for cx = 0 to w-1 do
        let cell = I.C.get_cell I.ctl cx cy in
        Termbox.set_cell_char ~fg:cell.fg ~bg:cell.bg (x + cx) (y + cy) cell.ch
      done
    done
  );
  Termbox.present ()


type state = {
  secrets : Secrets.t;
  input_ctl : Input.t;
  results_ctl : Label.t;
  controls : (module Control_instance) list;
  results: qres list
}


let rec loop state =
  render_controls state.controls;
  match Termbox.poll_event () with
  | Ascii c when c = '\x03' (* CTRL_C *) -> ()
  | (Ascii _ | Key _ | Utf8 _) as e ->
      (match Input.handle_event state.input_ctl e with
      | Some query ->
        let results = Secrets.search state.secrets query in
        let rlen = List.length results in
        let lines = Array.create rlen "" in
        let hl = Array.create rlen (Set.empty Int.comparator) in
        List.iteri results ~f:(fun i { summary; summary_hl; _ } ->
          lines.(i) <- summary;
          hl.(i) <- summary_hl);
        Label.update state.results_ctl lines ~hl;
        loop { state with results }
      | None -> loop state)
  | Resize _ -> loop state


let start secrets =
  ignore (Termbox.init ());
  let winw = Termbox.width () in
  let winh = Termbox.height () in
  let input_ctl = Input.create ((winw - 6), 1) in
  let results_ctl = Label.create ((winw - 1), (winh - 1)) [||] in
  let controls = [
    make_control_instance (module Label) (0, 0) (Label.create (6, 1) [|"find: "|]);
    make_control_instance (module Input) (6, 0) input_ctl;
    make_control_instance (module Label) (6, 1) results_ctl
  ] in
  loop { secrets; input_ctl; results_ctl; controls; results=[] };
  Termbox.shutdown ()
