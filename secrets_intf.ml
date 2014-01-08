module type Secrets_intf = sig
  type key
  type t

  val create : key -> string -> t
end
