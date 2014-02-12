open Core.Std

let pbcopy s =
  let po = Unix.open_process_out "pbcopy" in
  Out_channel.output_string po s;
  ignore (Unix.close_process_out po)
