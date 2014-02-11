open Core.Std


module Entry : sig
  type t
  val create : string -> (string * string) list -> t
  val title : t -> string
  val payload : t -> (string * string) list
  val to_string : t -> string
end = struct
  type t = { title : string; payload : (string * string) list }

  let title e = e.title
  let payload e = e.payload

  let create title payload = {title; payload}
  let to_string e =
    let open Bigbuffer in (
      let init_buff_len = (String.length e.title) * (List.length e.payload) in
      let buff = create init_buff_len in
      add_string buff e.title;
      add_char buff '\n';
      List.iter e.payload ~f:(fun (k, v) ->
        add_string buff k;
        add_string buff ": ";
        add_string buff v;
        add_char buff '\n'
      );
      contents buff
    )
end
