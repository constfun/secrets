external _link_me_ : unit -> unit = "YGNodeNew"

module PrintOptions = struct
  type t = Layout | Style | Children
  let write = function
    | Layout -> 1
    | Style -> 2
    | Children -> 4
  let read = function
    | 1 -> Layout
    | 2 -> Style
    | 4 -> Children
    | _ -> raise (Invalid_argument "Unexpected C enum")
  let typ = Ctypes.view ~read ~write Ctypes.int
end

module YGUnit = struct
  type t = Undefined | Point | Percent | Auto
  let write = function
    | Undefined -> 0
    | Point -> 1
    | Percent -> 2
    | Auto -> 4
  let read = function
    | 0 -> Undefined
    | 1 -> Point
    | 2 -> Percent
    | 4 -> Auto
    | _ -> raise (Invalid_argument "Unexpected C enum")
  let typ = Ctypes.view ~read ~write Ctypes.int
end

open Ctypes
open Foreign

type yg_val
let yg_val = structure "YGValue"
let yg_val_value = field yg_val "value" float
let yg_val_unit = field yg_val "unit" YGUnit.typ
let () = seal (yg_val : yg_val structure typ)

type node_ref = unit ptr
let node_ref : node_ref typ = ptr void
let node_new = foreign "YGNodeNew" (void @-> returning node_ref)
let node_reset = foreign "YGNodeReset" (node_ref @-> returning void)

let node_style_set_width = foreign "YGNodeStyleSetWidth" (node_ref @-> float @-> returning void)
let node_style_get_width = foreign "YGNodeStyleGetWidth" (node_ref @-> returning yg_val)

let node_style_set_height = foreign "YGNodeStyleSetHeight" (node_ref @-> float @-> returning void)
let node_style_get_height = foreign "YGNodeStyleGetHeight" (node_ref @-> returning yg_val)

let node_style_get_min_width = foreign "YGNodeStyleGetMinWidth" (node_ref @-> returning float)


let node_print = foreign "YGNodePrint" (node_ref @-> PrintOptions.typ @-> returning void)
