open Core
open ExtUnix.Specific
open ExtUnix.Specific.Uname

let pbcopy s =
  let unamerec = uname () in
  let os = unamerec.sysname in
  let utility = match os with
  | "Linux" -> begin match Sys.getenv "WAYLAND_DISPLAY" with 
    | Some _ -> "wl-copy"
    | None -> "xclip -selection clipboard"
  end
  | _ -> "pbcopy"
  in
  let po = Unix.open_process_out utility in
  Out_channel.output_string po s;
  ignore (Unix.close_process_out po)
