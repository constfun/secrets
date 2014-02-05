open Core.Std

type color =
  | Default
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

type event =
  | Key of char
  | Resize of int * int

module Termbox : sig
  val init : unit -> int
  val shutdown : unit -> unit

  val width : unit -> int
  val height : unit -> int

  val clear : unit -> unit
  val set_clear_attributes : color -> color -> unit

  val present : unit -> unit

  val set_cursor : int -> int -> unit
  val hide_cursor : unit -> unit

  val set_cell_char : ?fg : color -> ?bg : color -> int -> int -> char -> unit
  val set_cell_utf8 : ?fg : color -> ?bg : color -> int -> int -> int32 -> unit

  val poll_event : unit -> event
end = struct
  type cell = { ch : int; fg : color; bg : color }
  type buff = cell list

  external init : unit -> int = "tbstub_init"
  external shutdown : unit -> unit = "tb_shutdown"

  external width : unit -> int = "tbstub_width"
  external height : unit -> int = "tbstub_height"

  external clear : unit -> unit = "tb_clear"

  let int_of_color = function
    | Default -> 0x00
    | Black -> 0x01
    | Red -> 0x02
    | Green -> 0x03
    | Yellow -> 0x04
    | Blue -> 0x05
    | Magenta -> 0x06
    | Cyan -> 0x07
    | White -> 0x08

  external tb_set_clear_attributes : int -> int -> unit = "tbstub_set_clear_attributes"
  let set_clear_attributes fg bg =
    tb_set_clear_attributes (int_of_color fg) (int_of_color bg)

  external present : unit -> unit = "tb_present"

  external set_cursor : int -> int -> unit = "tbstub_set_cursor"

  let hide_cursor () =
    set_cursor (-1) (-1)

  external tb_change_cell : int -> int -> int32 -> int -> int -> unit = "tbstub_change_cell"

  let set_cell_utf8 ?(fg=Default) ?(bg=Default) x y ch =
    tb_change_cell x y ch (int_of_color fg) (int_of_color bg)

  let set_cell_char ?(fg=Default) ?(bg=Default) x y ch =
    let ch_int32 = Int32.of_int_exn (Char.to_int ch) in
    set_cell_utf8 ~fg ~bg x y ch_int32


  let tb_EVENT_KEY = 1
  let tb_EVENT_RESIZE = 2

  type tb_event = {
    _type : int;
    _mod : int;
    key : int;
    ch : int32;
    w : int;
    h : int;
  }
  external tb_poll_event : unit -> tb_event = "tbstub_poll_event"

  let rec poll_event () =
    let e = tb_poll_event () in
    match e._type with
    | tb_EVENT_KEY -> (
      match Option.((Int32.to_int e.ch) >>= Char.of_int) with
      | Some ch -> Key ch
      | None -> poll_event ()
    )
    | tb_EVENT_RESIZE -> Resize (e.w, e.h)
end
