open Gadt

module type K = Gadt.Hashtbl.KEY

module type S = sig
  type 'a key
  type kind = Pure | Io
  type 'v t

  val create : int -> 'v t
  val store : 'v t -> 'a key -> 'v -> kind -> unit
  val retrieve : 'v t -> 'a key -> ('v * kind) option
end

module Make (K : Hashtbl.KEY) : S with type 'a key = 'a K.t = struct
  module Tbl = Hashtbl.Make (K)

  type 'a key = 'a Tbl.key
  type kind = Pure | Io
  type 'v t = ('v * kind) Tbl.t

  let create n = Tbl.create ~random:false n
  let store tbl k v kind = Tbl.replace tbl k (v, kind)
  let retrieve = Tbl.find_opt
end
