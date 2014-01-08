module type Crypto_intf = sig
  type t

  module Key : sig
    type t
    val create : string -> t
  end

  val encrypt : Key.t -> string -> t
  val decrypt : Key.t -> t -> string

  val to_string : t -> string
  val of_string : string -> t
end
