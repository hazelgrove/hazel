/* Blocks: the regions of a program a blocks decoration shades, after Cohen
   and Chugh's ragged blocks. Each view (TypeBlocks) picks its
   own regions; this is what they have in common: a term, how deeply the
   block nests among the view's other blocks, and what it is painted. */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type paint =
  | Type(Typ.head);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  id: Id.t,
  /* the number of the view's blocks this one is inside; the root is 0 */
  level: int,
  paint,
};

/* The CSS class and color role a paint is drawn with. */
let paint_name: paint => string;
