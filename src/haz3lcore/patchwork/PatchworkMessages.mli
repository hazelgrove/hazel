[@@@ocaml.warning "-7-11-32-33-39"]
[@@@js.implem [@@@ocaml.warning "-7-11-32-33-39"]]

open Ts2ocaml
open Ts2ocaml.Dom

(* import type { HazelDoc } from "./flatdoc"; *)
[@@@js.stop]

module HazelDoc = FlatDoc.Export.HazelDoc

[@@@js.start]
[@@@js.implem module HazelDoc = FlatDoc.Export.HazelDoc]

(** Notification that a remote user disconnected - sent from parent to iframe.
    Iframe should remove the corresponding remote caret from display. *)
module RemoteCaretRemove : sig
  type t = [ `RemoteCaretRemove ] intf
  [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]

  type t_0 = t

  [@@@js.stop]

  type tags = [ `RemoteCaretRemove ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `RemoteCaretRemove ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `RemoteCaretRemove ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t :
    'tags this ->
    ([ `L_s6_remote_caret_remove [@js "remote-caret-remove"] ][@js.enum])
  [@@js.get "t"]

  val set_t :
    'tags this ->
    ([ `L_s6_remote_caret_remove [@js "remote-caret-remove"] ][@js.enum]) ->
    unit
  [@@js.set "t"]

  val get_userId : 'tags this -> string [@@js.get "userId"]
  val set_userId : 'tags this -> string -> unit [@@js.set "userId"]

  val create :
    t:([ `L_s6_remote_caret_remove [@js "remote-caret-remove"] ][@js.enum]) ->
    userId:string ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** Remote user's caret position - sent from parent to iframe. Contains user
    identification and styling info for rendering. *)
module RemoteCaret : sig
  type t = [ `RemoteCaret ] intf
  [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]

  type t_0 = t

  [@@@js.stop]

  type tags = [ `RemoteCaret ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `RemoteCaret ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `RemoteCaret ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t :
    'tags this -> ([ `L_s5_remote_caret [@js "remote-caret"] ][@js.enum])
  [@@js.get "t"]

  val set_t :
    'tags this ->
    ([ `L_s5_remote_caret [@js "remote-caret"] ][@js.enum]) ->
    unit
  [@@js.set "t"]

  val get_userId : 'tags this -> string [@@js.get "userId"]
  val set_userId : 'tags this -> string -> unit [@@js.set "userId"]
  val get_userName : 'tags this -> string option [@@js.get "userName"]
  val set_userName : 'tags this -> string -> unit [@@js.set "userName"]
  val get_color : 'tags this -> string [@@js.get "color"]
  val set_color : 'tags this -> string -> unit [@@js.set "color"]
  val get_pieceId : 'tags this -> string [@@js.get "pieceId"]
  val set_pieceId : 'tags this -> string -> unit [@@js.set "pieceId"]
  val get_shardIdx : 'tags this -> int option [@@js.get "shardIdx"]
  val set_shardIdx : 'tags this -> int option -> unit [@@js.set "shardIdx"]
  val get_caretOffset : 'tags this -> int [@@js.get "caretOffset"]
  val set_caretOffset : 'tags this -> int -> unit [@@js.set "caretOffset"]

  val get_shape :
    'tags this ->
    ([ `L_s2_left [@js "left"] | `L_s7_right [@js "right"] ][@js.enum]) option
  [@@js.get "shape"]

  val set_shape :
    'tags this ->
    ([ `Null
     | `U1 of ([ `L_s2_left [@js "left"] ][@js.enum])
     | `U2 of ([ `L_s7_right [@js "right"] ][@js.enum]) ]
    [@js.union]) ->
    unit
  [@@js.set "shape"]

  val get_side :
    'tags this ->
    ([ `L_s2_left [@js "left"] | `L_s7_right [@js "right"] ][@js.enum]) option
  [@@js.get "side"]

  val set_side :
    'tags this ->
    ([ `Null
     | `U1 of ([ `L_s2_left [@js "left"] ][@js.enum])
     | `U2 of ([ `L_s7_right [@js "right"] ][@js.enum]) ]
    [@js.union]) ->
    unit
  [@@js.set "side"]

  val create :
    t:([ `L_s5_remote_caret [@js "remote-caret"] ][@js.enum]) ->
    userId:string ->
    ?userName:string ->
    color:string ->
    pieceId:string ->
    ?shardIdx:int ->
    caretOffset:int ->
    ?shape:([ `L_s2_left [@js "left"] | `L_s7_right [@js "right"] ][@js.enum]) ->
    ?side:([ `L_s2_left [@js "left"] | `L_s7_right [@js "right"] ][@js.enum]) ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** Response to ping *)
module Pong : sig
  type t = [ `Pong ] intf [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]
  type t_0 = t

  [@@@js.stop]

  type tags = [ `Pong ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `Pong ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `Pong ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t : 'tags this -> ([ `L_s4_pong [@js "pong"] ][@js.enum])
  [@@js.get "t"]

  val set_t : 'tags this -> ([ `L_s4_pong [@js "pong"] ][@js.enum]) -> unit
  [@@js.set "t"]

  val get_message : 'tags this -> string [@@js.get "message"]
  val set_message : 'tags this -> string -> unit [@@js.set "message"]

  val create :
    t:([ `L_s4_pong [@js "pong"] ][@js.enum]) -> message:string -> unit -> t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** Connection test message *)
module Ping : sig
  type t = [ `Ping ] intf [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]
  type t_0 = t

  [@@@js.stop]

  type tags = [ `Ping ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `Ping ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `Ping ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t : 'tags this -> ([ `L_s3_ping [@js "ping"] ][@js.enum])
  [@@js.get "t"]

  val set_t : 'tags this -> ([ `L_s3_ping [@js "ping"] ][@js.enum]) -> unit
  [@@js.set "t"]

  val get_message : 'tags this -> string [@@js.get "message"]
  val set_message : 'tags this -> string -> unit [@@js.set "message"]

  val create :
    t:([ `L_s3_ping [@js "ping"] ][@js.enum]) -> message:string -> unit -> t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** The main sync message - contains document state delta.

    - `state`: Changed/added pieces (partial HazelDoc with only affected pieces)
    - `deleted`: IDs of pieces to remove from Automerge

    Why explicit deletion? Hazel uses a tree structure where deleted pieces
    simply disappear. Automerge uses a flat map where pieces persist unless
    explicitly removed. Without explicit deletion, deleted pieces become
    "orphans" in Automerge, causing undo/redo sync to fail: when undo restores a
    piece, it's already in Automerge (unchanged), so it's not forwarded to other
    clients, who then crash when the parent references a missing piece.

    See docs/patchwork-integration.md "Explicit Deletion Sync" for details. *)
module EditorState : sig
  type t = [ `EditorState ] intf
  [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]

  type t_0 = t

  [@@@js.stop]

  type tags = [ `EditorState ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `EditorState ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `EditorState ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t : 'tags this -> ([ `L_s8_state [@js "state"] ][@js.enum])
  [@@js.get "t"]

  val set_t : 'tags this -> ([ `L_s8_state [@js "state"] ][@js.enum]) -> unit
  [@@js.set "t"]

  val get_state : 'tags this -> HazelDoc.t_0 [@@js.get "state"]
  val set_state : 'tags this -> HazelDoc.t_0 -> unit [@@js.set "state"]
  val get_deleted : 'tags this -> string list option [@@js.get "deleted"]
  val set_deleted : 'tags this -> string list -> unit [@@js.set "deleted"]

  val create :
    t:([ `L_s8_state [@js "state"] ][@js.enum]) ->
    state:HazelDoc.t_0 ->
    ?deleted:string list ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** Messages sent from parent (Patchwork) to Hazel iframe *)
module ParentToHazel : sig
  type t =
    ([ `U_s3_ping of Ping.t [@js "ping"]
     | `U_s4_pong of Pong.t [@js "pong"]
     | `U_s5_remote_caret of RemoteCaret.t [@js "remote-caret"]
     | `U_s6_remote_caret_remove of RemoteCaretRemove.t
       [@js "remote-caret-remove"]
     | `U_s8_state of EditorState.t [@js "state"] ]
    [@js.union on_field "t"])

  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

(** Sent when iframe loads to signal readiness to receive state *)
module Init : sig
  type t = [ `Init ] intf [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]
  type t_0 = t

  [@@@js.stop]

  type tags = [ `Init ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `Init ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `Init ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t : 'tags this -> ([ `L_s1_init [@js "init"] ][@js.enum])
  [@@js.get "t"]

  val set_t : 'tags this -> ([ `L_s1_init [@js "init"] ][@js.enum]) -> unit
  [@@js.set "t"]

  val get_message : 'tags this -> string [@@js.get "message"]
  val set_message : 'tags this -> string -> unit [@@js.set "message"]

  val create :
    t:([ `L_s1_init [@js "init"] ][@js.enum]) -> message:string -> unit -> t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** Caret position update - sent from Hazel iframe when local caret moves. Used
    for collaborative cursor display via ephemeral broadcast.

    Position model:
    - pieceId: ID of the piece the caret is "on" (first of right siblings, or
      last of left if at end)
    - shardIdx: For tiles, which shard (delimiter) of the tile. null for
      non-tiles. Multi-shard tiles (let/in, if/then/else) share one ID across
      all shards. We need shardFlatDoc to look up the correct shard's
      measurement.
    - caretOffset: 0 = Outer (at piece's left edge), n = Inner(n-1) (n columns
      into the piece)
    - shape: Caret shape for rendering at piece boundaries (null when inside a
      piece)
    - side: Which edge of the piece the caret is on when at Outer position.
      "left" = caret is at left edge of piece (normal case, piece is to the
      right) "right" = caret is at right edge of piece (end-of-segment, piece is
      to the left) null = caret is inside the piece (Inner position) *)
module CaretUpdate : sig
  type t = [ `CaretUpdate ] intf
  [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]

  type t_0 = t

  [@@@js.stop]

  type tags = [ `CaretUpdate ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `CaretUpdate ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `CaretUpdate ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t : 'tags this -> ([ `L_s0_caret [@js "caret"] ][@js.enum])
  [@@js.get "t"]

  val set_t : 'tags this -> ([ `L_s0_caret [@js "caret"] ][@js.enum]) -> unit
  [@@js.set "t"]

  val get_pieceId : 'tags this -> string [@@js.get "pieceId"]
  val set_pieceId : 'tags this -> string -> unit [@@js.set "pieceId"]
  val get_shardIdx : 'tags this -> int option [@@js.get "shardIdx"]
  val set_shardIdx : 'tags this -> int option -> unit [@@js.set "shardIdx"]
  val get_caretOffset : 'tags this -> int [@@js.get "caretOffset"]
  val set_caretOffset : 'tags this -> int -> unit [@@js.set "caretOffset"]

  val get_shape :
    'tags this ->
    ([ `L_s2_left [@js "left"] | `L_s7_right [@js "right"] ][@js.enum]) option
  [@@js.get "shape"]

  val set_shape :
    'tags this ->
    ([ `Null
     | `U1 of ([ `L_s2_left [@js "left"] ][@js.enum])
     | `U2 of ([ `L_s7_right [@js "right"] ][@js.enum]) ]
    [@js.union]) ->
    unit
  [@@js.set "shape"]

  val get_side :
    'tags this ->
    ([ `L_s2_left [@js "left"] | `L_s7_right [@js "right"] ][@js.enum]) option
  [@@js.get "side"]

  val set_side :
    'tags this ->
    ([ `Null
     | `U1 of ([ `L_s2_left [@js "left"] ][@js.enum])
     | `U2 of ([ `L_s7_right [@js "right"] ][@js.enum]) ]
    [@js.union]) ->
    unit
  [@@js.set "side"]

  val create :
    t:([ `L_s0_caret [@js "caret"] ][@js.enum]) ->
    pieceId:string ->
    ?shardIdx:int ->
    caretOffset:int ->
    ?shape:([ `L_s2_left [@js "left"] | `L_s7_right [@js "right"] ][@js.enum]) ->
    ?side:([ `L_s2_left [@js "left"] | `L_s7_right [@js "right"] ][@js.enum]) ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** Messages sent from Hazel iframe to parent (Patchwork) *)
module HazelToParent : sig
  type t =
    ([ `U_s0_caret of CaretUpdate.t [@js "caret"]
     | `U_s1_init of Init.t [@js "init"]
     | `U_s3_ping of Ping.t [@js "ping"]
     | `U_s4_pong of Pong.t [@js "pong"]
     | `U_s8_state of EditorState.t [@js "state"] ]
    [@js.union on_field "t"])

  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

module Export : sig
  (* export interface Init *)
  [@@@js.stop]

  module Init = Init

  [@@@js.start]
  [@@@js.implem module Init = Init]

  (* export interface Ping *)
  [@@@js.stop]

  module Ping = Ping

  [@@@js.start]
  [@@@js.implem module Ping = Ping]

  (* export interface Pong *)
  [@@@js.stop]

  module Pong = Pong

  [@@@js.start]
  [@@@js.implem module Pong = Pong]

  (* export interface EditorState *)
  [@@@js.stop]

  module EditorState = EditorState

  [@@@js.start]
  [@@@js.implem module EditorState = EditorState]

  (* export interface CaretUpdate *)
  [@@@js.stop]

  module CaretUpdate = CaretUpdate

  [@@@js.start]
  [@@@js.implem module CaretUpdate = CaretUpdate]

  (* export interface RemoteCaret *)
  [@@@js.stop]

  module RemoteCaret = RemoteCaret

  [@@@js.start]
  [@@@js.implem module RemoteCaret = RemoteCaret]

  (* export interface RemoteCaretRemove *)
  [@@@js.stop]

  module RemoteCaretRemove = RemoteCaretRemove

  [@@@js.start]
  [@@@js.implem module RemoteCaretRemove = RemoteCaretRemove]

  (* export type HazelToParent *)
  [@@@js.stop]

  module HazelToParent = HazelToParent

  [@@@js.start]
  [@@@js.implem module HazelToParent = HazelToParent]

  (* export type ParentToHazel *)
  [@@@js.stop]

  module ParentToHazel = ParentToHazel

  [@@@js.start]
  [@@@js.implem module ParentToHazel = ParentToHazel]
end
