/**
 * A decomposition of `Layout` into box shapes that can be easily
 * translated into HTML boxes. In the process of converting an
 * annotated `Layout` node into a `Box`, the `Layout` node may be
 * split into up to three boxes (head, body, tail) with the
 * annotation distributed over the constituents.
 */
[@deriving sexp]
type t('annot) =
  | Text(string)
  | HBox(list(t('annot)))
  | VBox(list(t('annot)))
  | Annot('annot, t('annot));

module Make: (MemoTbl.S) => {let mk: Layout.t('annot) => t('annot);};
