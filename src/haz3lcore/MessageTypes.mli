[@@@ocaml.warning "-7-11-32-33-39"]
[@@@js.implem 
  [@@@ocaml.warning "-7-11-32-33-39"]
]
open Ts2ocaml
open Ts2ocaml.Dom

(* import type { EditScript } from "./delta"; *)
[@@@js.stop] module EditScript = Index.Export.EditScript [@@@js.start] [@@@js.implem module EditScript = Index.Export.EditScript]

module Pong : sig
  type t = [`Pong] intf [@@js.custom { of_js=Obj.magic; to_js=Obj.magic }]
  type t_0 = t
  [@@@js.stop]
  type tags = [`Pong]
  type tags_0 = tags
  [@@@js.start]
  [@@@js.implem 
    type tags = [`Pong]
    type tags_0 = tags
  ]
  type 'tags this = 'tags intf constraint 'tags = [> `Pong ]
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
  val get_t: 'tags this -> ([`L_s3_pong[@js "pong"]] [@js.enum]) [@@js.get "t"]
  val set_t: 'tags this -> ([`L_s3_pong[@js "pong"]] [@js.enum]) -> unit [@@js.set "t"]
  val get_message: 'tags this -> string [@@js.get "message"]
  val set_message: 'tags this -> string -> unit [@@js.set "message"]
  val create: t:([`L_s3_pong[@js "pong"]] [@js.enum]) -> message:string -> unit -> t [@@js.builder]
  val cast_from: 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end
module Ping : sig
  type t = [`Ping] intf [@@js.custom { of_js=Obj.magic; to_js=Obj.magic }]
  type t_0 = t
  [@@@js.stop]
  type tags = [`Ping]
  type tags_0 = tags
  [@@@js.start]
  [@@@js.implem 
    type tags = [`Ping]
    type tags_0 = tags
  ]
  type 'tags this = 'tags intf constraint 'tags = [> `Ping ]
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
  val get_t: 'tags this -> ([`L_s2_ping[@js "ping"]] [@js.enum]) [@@js.get "t"]
  val set_t: 'tags this -> ([`L_s2_ping[@js "ping"]] [@js.enum]) -> unit [@@js.set "t"]
  val get_message: 'tags this -> string [@@js.get "message"]
  val set_message: 'tags this -> string -> unit [@@js.set "message"]
  val create: t:([`L_s2_ping[@js "ping"]] [@js.enum]) -> message:string -> unit -> t [@@js.builder]
  val cast_from: 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end
module Init : sig
  type t = [`Init] intf [@@js.custom { of_js=Obj.magic; to_js=Obj.magic }]
  type t_0 = t
  [@@@js.stop]
  type tags = [`Init]
  type tags_0 = tags
  [@@@js.start]
  [@@@js.implem 
    type tags = [`Init]
    type tags_0 = tags
  ]
  type 'tags this = 'tags intf constraint 'tags = [> `Init ]
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
  val get_t: 'tags this -> ([`L_s1_init[@js "init"]] [@js.enum]) [@@js.get "t"]
  val set_t: 'tags this -> ([`L_s1_init[@js "init"]] [@js.enum]) -> unit [@@js.set "t"]
  val get_message: 'tags this -> string [@@js.get "message"]
  val set_message: 'tags this -> string -> unit [@@js.set "message"]
  val create: t:([`L_s1_init[@js "init"]] [@js.enum]) -> message:string -> unit -> t [@@js.builder]
  val cast_from: 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end
module EditorDelta : sig
  type t = [`EditorDelta] intf [@@js.custom { of_js=Obj.magic; to_js=Obj.magic }]
  type t_0 = t
  [@@@js.stop]
  type tags = [`EditorDelta]
  type tags_0 = tags
  [@@@js.start]
  [@@@js.implem 
    type tags = [`EditorDelta]
    type tags_0 = tags
  ]
  type 'tags this = 'tags intf constraint 'tags = [> `EditorDelta ]
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
  val get_t: 'tags this -> ([`L_s0_delta[@js "delta"]] [@js.enum]) [@@js.get "t"]
  val set_t: 'tags this -> ([`L_s0_delta[@js "delta"]] [@js.enum]) -> unit [@@js.set "t"]
  val get_delta: 'tags this -> EditScript.t_0 [@@js.get "delta"]
  val set_delta: 'tags this -> EditScript.t_0 -> unit [@@js.set "delta"]
  val create: t:([`L_s0_delta[@js "delta"]] [@js.enum]) -> delta:EditScript.t_0 -> unit -> t [@@js.builder]
  val cast_from: 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end
module ParentToHazel : sig
  type t = ([`U_s0_delta of EditorDelta.t [@js "delta"] | `U_s1_init of Init.t [@js "init"] | `U_s2_ping of Ping.t [@js "ping"] | `U_s3_pong of Pong.t [@js "pong"]] [@js.union on_field "t"])
  type t_0 = t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
end
module HazelToParent : sig
  type t = ([`U_s0_delta of EditorDelta.t [@js "delta"] | `U_s1_init of Init.t [@js "init"] | `U_s2_ping of Ping.t [@js "ping"] | `U_s3_pong of Pong.t [@js "pong"]] [@js.union on_field "t"])
  type t_0 = t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
end

module Export : sig
  (* export interface Init *)
  [@@@js.stop] module Init = Init [@@@js.start] [@@@js.implem module Init = Init]
  (* export interface Ping *)
  [@@@js.stop] module Ping = Ping [@@@js.start] [@@@js.implem module Ping = Ping]
  (* export interface Pong *)
  [@@@js.stop] module Pong = Pong [@@@js.start] [@@@js.implem module Pong = Pong]
  (* export interface EditorDelta *)
  [@@@js.stop] module EditorDelta = EditorDelta [@@@js.start] [@@@js.implem module EditorDelta = EditorDelta]
  (* export type HazelToParent *)
  [@@@js.stop] module HazelToParent = HazelToParent [@@@js.start] [@@@js.implem module HazelToParent = HazelToParent]
  (* export type ParentToHazel *)
  [@@@js.stop] module ParentToHazel = ParentToHazel [@@@js.start] [@@@js.implem module ParentToHazel = ParentToHazel]
end