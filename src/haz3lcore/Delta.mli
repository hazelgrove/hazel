[@@@ocaml.warning "-7-11-32-33-39"]
[@@@js.implem [@@@ocaml.warning "-7-11-32-33-39"]]

open Ts2ocaml
open Ts2ocaml.Dom

module UUID : sig
  type t = string
  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

module Sort : sig
  type t = ([ `L_s2_Exp [@js "Exp"] ][@js.enum])
  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

module Shape : sig
  type t = ([ `L_s1_Convex [@js "Convex"] ][@js.enum])
  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

module Nib : sig
  type t = [ `Nib ] intf [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]
  type t_0 = t

  [@@@js.stop]

  type tags = [ `Nib ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `Nib ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `Nib ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
  val get_shape : 'tags this -> Shape.t [@@js.get "shape"]
  val set_shape : 'tags this -> Shape.t -> unit [@@js.set "shape"]
  val get_sort : 'tags this -> Sort.t [@@js.get "sort"]
  val set_sort : 'tags this -> Sort.t -> unit [@@js.set "sort"]
  val create : shape:Shape.t -> sort:Sort.t -> unit -> t [@@js.builder]
  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

module Mold : sig
  type t = [ `Mold ] intf [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]
  type t_0 = t

  [@@@js.stop]

  type tags = [ `Mold ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `Mold ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `Mold ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
  val get_out : 'tags this -> Sort.t [@@js.get "out"]
  val set_out : 'tags this -> Sort.t -> unit [@@js.set "out"]
  val get_in : 'tags this -> Sort.t list [@@js.get "in"]
  val set_in : 'tags this -> Sort.t list -> unit [@@js.set "in"]
  val get_nibs : 'tags this -> Nib.t * Nib.t [@@js.get "nibs"]
  val set_nibs : 'tags this -> Nib.t * Nib.t -> unit [@@js.set "nibs"]

  val create :
    out:Sort.t -> in_:(Sort.t list[@js "in"]) -> nibs:Nib.t * Nib.t -> unit -> t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

module Tile : sig
  type t = [ `Tile ] intf [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]
  type t_0 = t

  [@@@js.stop]

  type tags = [ `Tile ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `Tile ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `Tile ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t : 'tags this -> ([ `L_s5_Tile [@js "Tile"] ][@js.enum])
  [@@js.get "t"]

  val get_id : 'tags this -> UUID.t [@@js.get "id"]
  val get_label : 'tags this -> string list [@@js.get "label"]
  val get_mold : 'tags this -> Mold.t [@@js.get "mold"]
  val get_shards : 'tags this -> float list [@@js.get "shards"]
  val get_children : 'tags this -> t list [@@js.get "children"]

  val create :
    t:([ `L_s5_Tile [@js "Tile"] ][@js.enum]) ->
    id:UUID.t ->
    label:string list ->
    mold:Mold.t ->
    shards:float list ->
    children:t list ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

module SecondaryContent : sig
  type t = [ `SecondaryContent ] intf
  [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]

  type t_0 = t

  [@@@js.stop]

  type tags = [ `SecondaryContent ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `SecondaryContent ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `SecondaryContent ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t :
    'tags this ->
    ([ `L_s0_Comment [@js "Comment"] | `L_s6_Whitespace [@js "Whitespace"] ]
    [@js.enum])
  [@@js.get "t"]

  val get_content : 'tags this -> string [@@js.get "content"]

  val create :
    t:
      ([ `L_s0_Comment [@js "Comment"] | `L_s6_Whitespace [@js "Whitespace"] ]
      [@js.enum]) ->
    content:string ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

module Secondary : sig
  type t = [ `Secondary ] intf
  [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]

  type t_0 = t

  [@@@js.stop]

  type tags = [ `Secondary ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `Secondary ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `Secondary ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t : 'tags this -> ([ `L_s4_Secondary [@js "Secondary"] ][@js.enum])
  [@@js.get "t"]

  val get_id : 'tags this -> UUID.t [@@js.get "id"]
  val get_content : 'tags this -> SecondaryContent.t [@@js.get "content"]

  val create :
    t:([ `L_s4_Secondary [@js "Secondary"] ][@js.enum]) ->
    id:UUID.t ->
    content:SecondaryContent.t ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

module Grout : sig
  type t = [ `Grout ] intf
  [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]

  type t_0 = t

  [@@@js.stop]

  type tags = [ `Grout ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `Grout ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `Grout ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t : 'tags this -> ([ `L_s3_Grout [@js "Grout"] ][@js.enum])
  [@@js.get "t"]

  val get_id : 'tags this -> UUID.t [@@js.get "id"]
  val get_shape : 'tags this -> Shape.t [@@js.get "shape"]

  val create :
    t:([ `L_s3_Grout [@js "Grout"] ][@js.enum]) ->
    id:UUID.t ->
    shape:Shape.t ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

module TileId : sig
  type t = (* FIXME: unknown type '`TILE:${UUID}`' *) any
  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

module FlatTile : sig
  type t = [ `FlatTile ] intf
  [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]

  type t_0 = t

  [@@@js.stop]

  type tags = [ `FlatTile ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `FlatTile ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `FlatTile ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t : 'tags this -> ([ `L_s5_Tile [@js "Tile"] ][@js.enum])
  [@@js.get "t"]

  val get_id : 'tags this -> TileId.t [@@js.get "id"]
  val get_label : 'tags this -> string list [@@js.get "label"]
  val get_mold : 'tags this -> Mold.t [@@js.get "mold"]
  val get_shards : 'tags this -> float list [@@js.get "shards"]
  val get_children : 'tags this -> TileId.t list list [@@js.get "children"]

  val create :
    t:([ `L_s5_Tile [@js "Tile"] ][@js.enum]) ->
    id:TileId.t ->
    label:string list ->
    mold:Mold.t ->
    shards:float list ->
    children:TileId.t list list ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

module FlatPiece : sig
  type t =
    ([ `U_s3_Grout of Grout.t [@js "Grout"]
     | `U_s4_Secondary of Secondary.t [@js "Secondary"]
     | `U_s5_Tile of FlatTile.t [@js "Tile"] ]
    [@js.union on_field "t"])

  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

module HazelDoc : sig
  module AnonymousInterface0 : sig
    type t = private Ojs.t

    val t_to_js : t -> Ojs.t
    val t_of_js : Ojs.t -> t
    val get_title : t -> string [@@js.get "title"]
    val set_title : t -> string -> unit [@@js.set "title"]
    val get_tiles : t -> (TileId.t, FlatPiece.t) Map.t_2 [@@js.get "tiles"]

    val set_tiles : t -> (TileId.t, FlatPiece.t) Map.t_2 -> unit
    [@@js.set "tiles"]

    val get_root : t -> TileId.t [@@js.get "root"]
    val set_root : t -> TileId.t -> unit [@@js.set "root"]

    val create :
      title:string ->
      tiles:(TileId.t, FlatPiece.t) Map.t_2 ->
      root:TileId.t ->
      unit ->
      t
    [@@js.builder]
  end

  type t = AnonymousInterface0.t
  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

module Export : sig
  (* export type TileId *)
  [@@@js.stop]

  module TileId = TileId

  [@@@js.start]
  [@@@js.implem module TileId = TileId]

  (* export interface FlatTile *)
  [@@@js.stop]

  module FlatTile = FlatTile

  [@@@js.start]
  [@@@js.implem module FlatTile = FlatTile]

  (* export type FlatPiece *)
  [@@@js.stop]

  module FlatPiece = FlatPiece

  [@@@js.start]
  [@@@js.implem module FlatPiece = FlatPiece]

  (* export type HazelDoc *)
  [@@@js.stop]

  module HazelDoc = HazelDoc

  [@@@js.start]
  [@@@js.implem module HazelDoc = HazelDoc]
end
