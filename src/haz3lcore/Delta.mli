[@@@ocaml.warning "-7-11-32-33-39"]
[@@@js.implem 
  [@@@ocaml.warning "-7-11-32-33-39"]
]
open Ts2ocaml
open Ts2ocaml.Dom

module Sort : sig
  type t = ([`L_s2_Exp[@js "Exp"]] [@js.enum])
  type t_0 = t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
end
module Shape : sig
  type t = ([`L_s0_Convex[@js "Convex"]] [@js.enum])
  type t_0 = t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
end
module Nib : sig
  type t = [`Nib] intf [@@js.custom { of_js=Obj.magic; to_js=Obj.magic }]
  type t_0 = t
  [@@@js.stop]
  type tags = [`Nib]
  type tags_0 = tags
  [@@@js.start]
  [@@@js.implem 
    type tags = [`Nib]
    type tags_0 = tags
  ]
  type 'tags this = 'tags intf constraint 'tags = [> `Nib ]
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
  val get_shape: 'tags this -> Shape.t [@@js.get "shape"]
  val set_shape: 'tags this -> Shape.t -> unit [@@js.set "shape"]
  val get_sort: 'tags this -> Sort.t [@@js.get "sort"]
  val set_sort: 'tags this -> Sort.t -> unit [@@js.set "sort"]
  val create: shape:Shape.t -> sort:Sort.t -> unit -> t [@@js.builder]
  val cast_from: 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end
module Mold : sig
  type t = [`Mold] intf [@@js.custom { of_js=Obj.magic; to_js=Obj.magic }]
  type t_0 = t
  [@@@js.stop]
  type tags = [`Mold]
  type tags_0 = tags
  [@@@js.start]
  [@@@js.implem 
    type tags = [`Mold]
    type tags_0 = tags
  ]
  type 'tags this = 'tags intf constraint 'tags = [> `Mold ]
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
  val get_out: 'tags this -> Sort.t [@@js.get "out"]
  val set_out: 'tags this -> Sort.t -> unit [@@js.set "out"]
  val get_in: 'tags this -> Sort.t list [@@js.get "in"]
  val set_in: 'tags this -> Sort.t list -> unit [@@js.set "in"]
  val get_nibs: 'tags this -> (Nib.t * Nib.t) [@@js.get "nibs"]
  val set_nibs: 'tags this -> (Nib.t * Nib.t) -> unit [@@js.set "nibs"]
  val create: out:Sort.t -> in_:(Sort.t list[@js "in"]) -> nibs:(Nib.t * Nib.t) -> unit -> t [@@js.builder]
  val cast_from: 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end
module UUID : sig
  type t = string
  type t_0 = t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
end
module Tile : sig
  type t = [`Tile] intf [@@js.custom { of_js=Obj.magic; to_js=Obj.magic }]
  type t_0 = t
  [@@@js.stop]
  type tags = [`Tile]
  type tags_0 = tags
  [@@@js.start]
  [@@@js.implem 
    type tags = [`Tile]
    type tags_0 = tags
  ]
  type 'tags this = 'tags intf constraint 'tags = [> `Tile ]
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
  val get_t: 'tags this -> ([`L_s4_Tile[@js "Tile"]] [@js.enum]) [@@js.get "t"]
  val get_id: 'tags this -> UUID.t [@@js.get "id"]
  val get_label: 'tags this -> string list [@@js.get "label"]
  val get_mold: 'tags this -> Mold.t [@@js.get "mold"]
  val get_shards: 'tags this -> float list [@@js.get "shards"]
  val get_children: 'tags this -> t list [@@js.get "children"]
  val create: t:([`L_s4_Tile[@js "Tile"]] [@js.enum]) -> id:UUID.t -> label:string list -> mold:Mold.t -> shards:float list -> children:t list -> unit -> t [@@js.builder]
  val cast_from: 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end
module InsertOp : sig
  type t = [`InsertOp] intf [@@js.custom { of_js=Obj.magic; to_js=Obj.magic }]
  type t_0 = t
  [@@@js.stop]
  type tags = [`InsertOp]
  type tags_0 = tags
  [@@@js.start]
  [@@@js.implem 
    type tags = [`InsertOp]
    type tags_0 = tags
  ]
  type 'tags this = 'tags intf constraint 'tags = [> `InsertOp ]
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
  val get_t: 'tags this -> ([`L_s3_Insert[@js "Insert"]] [@js.enum]) [@@js.get "t"]
  val get_uuid: 'tags this -> UUID.t [@@js.get "uuid"]
  val get_index: 'tags this -> float [@@js.get "index"]
  val get_tiles: 'tags this -> Tile.t list [@@js.get "tiles"]
  val create: t:([`L_s3_Insert[@js "Insert"]] [@js.enum]) -> uuid:UUID.t -> index:float -> tiles:Tile.t list -> unit -> t [@@js.builder]
  val cast_from: 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end
module DeleteOp : sig
  type t = [`DeleteOp] intf [@@js.custom { of_js=Obj.magic; to_js=Obj.magic }]
  type t_0 = t
  [@@@js.stop]
  type tags = [`DeleteOp]
  type tags_0 = tags
  [@@@js.start]
  [@@@js.implem 
    type tags = [`DeleteOp]
    type tags_0 = tags
  ]
  type 'tags this = 'tags intf constraint 'tags = [> `DeleteOp ]
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
  val get_t: 'tags this -> ([`L_s1_Delete[@js "Delete"]] [@js.enum]) [@@js.get "t"]
  val get_uuid: 'tags this -> UUID.t [@@js.get "uuid"]
  val get_index: 'tags this -> float [@@js.get "index"]
  val create: t:([`L_s1_Delete[@js "Delete"]] [@js.enum]) -> uuid:UUID.t -> index:float -> unit -> t [@@js.builder]
  val cast_from: 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end
module EditOp : sig
  type t = ([`U_s1_Delete of DeleteOp.t [@js "Delete"] | `U_s3_Insert of InsertOp.t [@js "Insert"]] [@js.union on_field "t"])
  type t_0 = t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
end
module EditScript : sig
  type t = EditOp.t list
  type t_0 = t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val t_0_to_js: t_0 -> Ojs.t
  val t_0_of_js: Ojs.t -> t_0
end

module Export : sig
  (* export type EditScript *)
  [@@@js.stop] module EditScript = EditScript [@@@js.start] [@@@js.implem module EditScript = EditScript]
end