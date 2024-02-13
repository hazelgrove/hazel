/**
 * A decomposition of `Layout` into box shapes that can be easily
 * translated into HTML boxes. In the process of converting an
 * annotated multiline `Layout` node into a `Box`, the `Layout`
 * node is split into three boxes (head, body, tail) with the
 * annotation distributed over them. For example, a layout node
 * of the form
 * (
 *   let x = 1 in
 *   x + 1
 * )
 * is split into
 * ---
 * |(|
 * -----------------
 * |  let x = 1 in |
 * |  x + 1        |
 * -----------------
 * |)|
 * ---
 * at the top level (each box may be further decomposed internally),
 * where each of the three depicted boxes is annotated with the
 * same annotation as the original layout node.
 */
[@deriving sexp]
type t('annot) =
  | Text(string)
  | HBox(list(t('annot)))
  | VBox(list(t('annot)))
  | Annot('annot, t('annot));

module Make: (MemoTbl.S) => {
                              let mk: Layout.t('annot) => t('annot);
                            };
