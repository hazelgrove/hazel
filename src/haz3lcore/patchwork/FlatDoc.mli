[@@@ocaml.warning "-7-11-32-33-39"]
[@@@js.implem [@@@ocaml.warning "-7-11-32-33-39"]]

open Ts2ocaml
open Ts2ocaml.Dom

(** Content of secondary (non-code) pieces *)
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
    ([ `L_s13_Whitespace [@js "Whitespace"] | `L_s1_Comment [@js "Comment"] ]
    [@js.enum])
  [@@js.get "t"]

  val get_content : 'tags this -> string [@@js.get "content"]

  val create :
    t:
      ([ `L_s13_Whitespace [@js "Whitespace"] | `L_s1_Comment [@js "Comment"] ]
      [@js.enum]) ->
    content:string ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** Hazel Document Types for Patchwork Integration

    These TypeScript types define the flat document structure used for sync
    between Hazel (running in an iframe) and the parent Patchwork application.

    Key design decision: Hazel's internal AST is a nested tree (Segment), but
    Automerge (used by Patchwork) works best with flat structures. So we use a
    "flattened" representation where tiles reference children by UUID instead of
    containing them directly.

    Type Conversion Flow: TypeScript (this file) --\[ts2ocaml\]--> OCaml
    (FlatDoc.mli)

    To regenerate OCaml types after modifying this file: cd embed && pnpm
    type:flatdoc

    The OCaml conversions in PatchworkComm.re (JsConvert module) handle:
    - of_* functions: OCaml types -> JS/FlatDoc types (for sending to parent)
    - to_* functions: JS/FlatDoc types -> OCaml types (for receiving from
      parent)

    Runtime conversion between nested Segment and flat Doc happens in
    FlatConvert.re:
    - seg_to_doc: Segment -> flat Doc (for sending)
    - doc_to_seg: flat Doc -> Segment (for receiving) *)
module UUID : sig
  type t = string
  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

(** Secondary represents whitespace or comments *)
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

  val get_t : 'tags this -> ([ `L_s9_Secondary [@js "Secondary"] ][@js.enum])
  [@@js.get "t"]

  val get_id : 'tags this -> UUID.t [@@js.get "id"]
  val get_content : 'tags this -> SecondaryContent.t [@@js.get "content"]

  val create :
    t:([ `L_s9_Secondary [@js "Secondary"] ][@js.enum]) ->
    id:UUID.t ->
    content:SecondaryContent.t ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** Shape of a grout piece (hole) *)
module Shape : sig
  type t =
    ([ `L_s2_Concave [@js "Concave"] | `L_s3_Convex [@js "Convex"] ][@js.enum])

  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

(** Grout represents a "hole" in the syntax - a placeholder for missing code *)
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

  val get_t : 'tags this -> ([ `L_s5_Grout [@js "Grout"] ][@js.enum])
  [@@js.get "t"]

  val get_id : 'tags this -> UUID.t [@@js.get "id"]
  val get_shape : 'tags this -> Shape.t [@@js.get "shape"]

  val create :
    t:([ `L_s5_Grout [@js "Grout"] ][@js.enum]) ->
    id:UUID.t ->
    shape:Shape.t ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** Hazel syntactic sorts - the grammatical categories of the language *)
module Sort : sig
  type t =
    ([ `L_s0_Any [@js "Any"]
     | `L_s10_TPat [@js "TPat"]
     | `L_s12_Typ [@js "Typ"]
     | `L_s4_Exp [@js "Exp"]
     | `L_s6_Pat [@js "Pat"]
     | `L_s8_Rul [@js "Rul"] ]
    [@js.enum])

  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

(** Shape of a nib (tile edge) - Concave has a precedence level n *)
module NibShape : sig
  module rec AnonymousInterface0 : sig
    type t = private Ojs.t

    val t_to_js : t -> Ojs.t
    val t_of_js : Ojs.t -> t

    val get_t : t -> ([ `L_s2_Concave [@js "Concave"] ][@js.enum])
    [@@js.get "t"]

    val set_t : t -> ([ `L_s2_Concave [@js "Concave"] ][@js.enum]) -> unit
    [@@js.set "t"]

    val get_n : t -> int [@@js.get "n"]
    val set_n : t -> int -> unit [@@js.set "n"]

    val create :
      t:([ `L_s2_Concave [@js "Concave"] ][@js.enum]) -> n:int -> unit -> t
    [@@js.builder]
  end

  and AnonymousInterface1 : sig
    type t = private Ojs.t

    val t_to_js : t -> Ojs.t
    val t_of_js : Ojs.t -> t
    val get_t : t -> ([ `L_s3_Convex [@js "Convex"] ][@js.enum]) [@@js.get "t"]

    val set_t : t -> ([ `L_s3_Convex [@js "Convex"] ][@js.enum]) -> unit
    [@@js.set "t"]

    val create : t:([ `L_s3_Convex [@js "Convex"] ][@js.enum]) -> unit -> t
    [@@js.builder]
  end

  type t =
    ([ `U_s2_Concave of AnonymousInterface0.t [@js "Concave"]
     | `U_s3_Convex of AnonymousInterface1.t [@js "Convex"] ]
    [@js.union on_field "t"])

  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

(** A nib is one edge of a tile, with a shape and sort *)
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
  val get_shape : 'tags this -> NibShape.t [@@js.get "shape"]
  val set_shape : 'tags this -> NibShape.t -> unit [@@js.set "shape"]
  val get_sort : 'tags this -> Sort.t [@@js.get "sort"]
  val set_sort : 'tags this -> Sort.t -> unit [@@js.set "sort"]
  val create : shape:NibShape.t -> sort:Sort.t -> unit -> t [@@js.builder]
  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** A mold describes a tile's "shape" - its output sort, input sorts, and edge
    nibs *)
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

(** FlatTile is the flattened representation of a syntax tile. Unlike the
    internal Tile which contains child Segments directly, FlatTile references
    children by UUID arrays (one array per child slot).

    Example: An "if" tile with 3 children (condition, then, else) would have:
    children: \[\[uuid1, uuid2\], \[uuid3\], \[uuid4, uuid5\]\] where each inner
    array is the sequence of piece UUIDs in that child slot. *)
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

  val get_t : 'tags this -> ([ `L_s11_Tile [@js "Tile"] ][@js.enum])
  [@@js.get "t"]

  val get_id : 'tags this -> UUID.t [@@js.get "id"]
  val get_label : 'tags this -> string list [@@js.get "label"]
  val get_mold : 'tags this -> Mold.t [@@js.get "mold"]
  val get_shards : 'tags this -> int list [@@js.get "shards"]
  val get_children : 'tags this -> UUID.t list list [@@js.get "children"]

  val create :
    t:([ `L_s11_Tile [@js "Tile"] ][@js.enum]) ->
    id:UUID.t ->
    label:string list ->
    mold:Mold.t ->
    shards:int list ->
    children:UUID.t list list ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** FlatProjector is the flattened representation of a projector. A projector
    wraps a piece of syntax with additional UI/behavior.

    The `syntax` field is the UUID of the wrapped piece (which is stored
    separately in the flat doc). The wrapped piece and its children are
    recursively included in the flat tiles array.

    Note on model sync: The `model` field is synced as an opaque string. To
    disable model sync (keep models local-only), modify FlatConvert.re to use an
    empty string when converting to flat format, and preserve the local model
    when converting back. *)
module FlatProjector : sig
  type t = [ `FlatProjector ] intf
  [@@js.custom { of_js = Obj.magic; to_js = Obj.magic }]

  type t_0 = t

  [@@@js.stop]

  type tags = [ `FlatProjector ]
  type tags_0 = tags

  [@@@js.start]

  [@@@js.implem
  type tags = [ `FlatProjector ]
  type tags_0 = tags]

  type 'tags this = 'tags intf constraint 'tags = [> `FlatProjector ]

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0

  val get_t : 'tags this -> ([ `L_s7_Projector [@js "Projector"] ][@js.enum])
  [@@js.get "t"]

  val get_id : 'tags this -> UUID.t [@@js.get "id"]
  val get_kind : 'tags this -> string [@@js.get "kind"]
  val get_syntax : 'tags this -> UUID.t [@@js.get "syntax"]
  val get_model : 'tags this -> string [@@js.get "model"]

  val create :
    t:([ `L_s7_Projector [@js "Projector"] ][@js.enum]) ->
    id:UUID.t ->
    kind:string ->
    syntax:UUID.t ->
    model:string ->
    unit ->
    t
  [@@js.builder]

  val cast_from : 'tags this -> t [@@js.custom let cast_from = Obj.magic]
end

(** A piece in the flat document - either a tile, grout, secondary, or projector
*)
module FlatPiece : sig
  type t =
    ([ `U_s5_Grout of Grout.t [@js "Grout"]
     | `U_s7_Projector of FlatProjector.t [@js "Projector"]
     | `U_s9_Secondary of Secondary.t [@js "Secondary"]
     | `U_s11_Tile of FlatTile.t [@js "Tile"] ]
    [@js.union on_field "t"])

  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

(** HazelDoc is the top-level document structure sent via PostMessage. It's a
    flat map of all pieces in the document (keyed by UUID), with relationships
    encoded via UUID references in FlatTile.children.

    Changed from Array to Map for Automerge performance - map updates are O(1)
    instead of O(n) array diffing. *)
module HazelDoc : sig
  module rec AnonymousInterface2 : sig
    module Pieces4 : sig
      type t = private Ojs.t

      val t_to_js : t -> Ojs.t
      val t_of_js : Ojs.t -> t
      val get : t -> string -> FlatPiece.t [@@js.index_get]
      val set : t -> string -> FlatPiece.t -> unit [@@js.index_set]
    end

    type t = private Ojs.t

    val t_to_js : t -> Ojs.t
    val t_of_js : Ojs.t -> t
    val get_title : t -> string [@@js.get "title"]
    val set_title : t -> string -> unit [@@js.set "title"]
    val get_pieces : t -> Pieces4.t [@@js.get "pieces"]
    val set_pieces : t -> Pieces4.t -> unit [@@js.set "pieces"]
    val create : title:string -> pieces:Pieces4.t -> unit -> t [@@js.builder]
  end

  and Pieces4 : sig
    type t = private Ojs.t

    val t_to_js : t -> Ojs.t
    val t_of_js : Ojs.t -> t
    val get : t -> string -> FlatPiece.t [@@js.index_get]
    val set : t -> string -> FlatPiece.t -> unit [@@js.index_set]
  end

  type t = AnonymousInterface2.t
  type t_0 = t

  val t_to_js : t -> Ojs.t
  val t_of_js : Ojs.t -> t
  val t_0_to_js : t_0 -> Ojs.t
  val t_0_of_js : Ojs.t -> t_0
end

module Export : sig
  (* export interface FlatTile *)
  [@@@js.stop]

  module FlatTile = FlatTile

  [@@@js.start]
  [@@@js.implem module FlatTile = FlatTile]

  (* export interface FlatProjector *)
  [@@@js.stop]

  module FlatProjector = FlatProjector

  [@@@js.start]
  [@@@js.implem module FlatProjector = FlatProjector]

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
