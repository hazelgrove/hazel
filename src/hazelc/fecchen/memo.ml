open Gadt

module type K = Gadt.Hashtbl.KEY

module type S = sig
  type 'a key
  type 'v t

  val create : int -> 'v t
  val store : 'v t -> 'a key -> 'v -> unit
  val retrieve : 'v t -> 'a key -> 'v option
end

module Make (K : Hashtbl.KEY) : S with type 'a key = 'a K.t = struct
  module Tbl = Hashtbl.Make (K)

  type 'a key = 'a Tbl.key
  type 'v t = 'v Tbl.t

  let create n = Tbl.create ~random:false n
  let store = Tbl.replace
  let retrieve = Tbl.find_opt
end
