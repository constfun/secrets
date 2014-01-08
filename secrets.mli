open Secrets_intf

module NaclSecrets : Secrets_intf with type key = Crypto.NaclCrypto.Key.t
