open Core.Std

let () =
  In_channel.iter_lines In_channel.stdin ~f:(fun l -> print_endline l)
