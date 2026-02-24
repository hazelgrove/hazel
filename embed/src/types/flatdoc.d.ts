/**
 * Hazel Document Types for Patchwork Integration
 *
 * These TypeScript types define the flat document structure used for sync between
 * Hazel (running in an iframe) and the parent Patchwork application.
 *
 * Key design decision: Hazel's internal AST is a nested tree (Segment), but
 * Automerge (used by Patchwork) works best with flat structures. So we use a
 * "flattened" representation where tiles reference children by UUID instead of
 * containing them directly.
 *
 * Type Conversion Flow:
 *   TypeScript (this file) --[ts2ocaml]--> OCaml (FlatDoc.mli)
 *
 * To regenerate OCaml types after modifying this file:
 *   cd embed && pnpm type:flatdoc
 *
 * The OCaml conversions in PatchworkComm.re (JsConvert module) handle:
 *   - of_* functions: OCaml types -> JS/FlatDoc types (for sending to parent)
 *   - to_* functions: JS/FlatDoc types -> OCaml types (for receiving from parent)
 *
 * Runtime conversion between nested Segment and flat Doc happens in FlatConvert.re:
 *   - seg_to_doc: Segment -> flat Doc (for sending)
 *   - doc_to_seg: flat Doc -> Segment (for receiving)
 */

type UUID = string;

/** Hazel syntactic sorts - the grammatical categories of the language */
type Sort = "Exp" | "Pat" | "Typ" | "TPat" | "Rul" | "Any";

/** Shape of a grout piece (hole) */
type Shape = "Convex" | "Concave";

/** Shape of a nib (tile edge) - Concave has a precedence level n */
type NibShape = {t:"Convex"} | {t:"Concave", n:number};

/** A nib is one edge of a tile, with a shape and sort */
interface Nib {
  shape: NibShape;
  sort: Sort;
}

/** A mold describes a tile's "shape" - its output sort, input sorts, and edge nibs */
interface Mold {
  out: Sort;
  in: Sort[];
  nibs: [Nib, Nib];
}

/** Grout represents a "hole" in the syntax - a placeholder for missing code */
interface Grout {
  readonly t: "Grout";
  readonly id: UUID;
  readonly shape: Shape;
}

/** Content of secondary (non-code) pieces */
interface SecondaryContent {
  readonly t: "Whitespace" | "Comment";
  readonly content: string;
}

/** Secondary represents whitespace or comments */
interface Secondary {
  readonly t: "Secondary";
  readonly id: UUID;
  readonly content: SecondaryContent;
}

/**
 * FlatTile is the flattened representation of a syntax tile.
 * Unlike the internal Tile which contains child Segments directly,
 * FlatTile references children by UUID arrays (one array per child slot).
 *
 * Example: An "if" tile with 3 children (condition, then, else) would have:
 *   children: [[uuid1, uuid2], [uuid3], [uuid4, uuid5]]
 * where each inner array is the sequence of piece UUIDs in that child slot.
 */
export interface FlatTile {
  readonly t: "Tile";
  readonly id: UUID;
  readonly label: string[];   // Token labels, e.g. ["if", "then", "else"]
  readonly mold: Mold;
  readonly shards: number[];  // Which label indices are present (for partial tiles)
  readonly children: UUID[][]; // Child piece IDs, grouped by child slot
}

/**
 * FlatProjector is the flattened representation of a projector.
 * A projector wraps a piece of syntax with additional UI/behavior.
 *
 * The `syntax` field is the UUID of the wrapped piece (which is stored
 * separately in the flat doc). The wrapped piece and its children are
 * recursively included in the flat tiles array.
 *
 * Note on model sync: The `model` field is synced as an opaque string.
 * To disable model sync (keep models local-only), modify FlatConvert.re
 * to use an empty string when converting to flat format, and preserve
 * the local model when converting back.
 */
export interface FlatProjector {
  readonly t: "Projector";
  readonly id: UUID;
  readonly kind: string;    // Projector kind name: "fold", "check", "slider", etc.
  readonly syntax: UUID;    // ID of the wrapped piece
  readonly model: string;   // Projector model state (opaque, typically JSON)
}

/** A piece in the flat document - either a tile, grout, secondary, or projector */
export type FlatPiece = FlatTile | Grout | Secondary | FlatProjector;

/**
 * HazelDoc is the top-level document structure sent via PostMessage.
 * It's a flat map of all pieces in the document (keyed by UUID), with
 * relationships encoded via UUID references in FlatTile.children.
 *
 * Changed from Array to Map for Automerge performance - map updates are
 * O(1) instead of O(n) array diffing.
 */
export type HazelDoc = {
    title: string;
    pieces: { [id: string]: FlatPiece };
}
