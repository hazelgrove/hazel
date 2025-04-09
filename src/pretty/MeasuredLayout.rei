[@deriving sexp]
type box = {
  height: int,
  width: int,
};

/**
 * Augmented version of `Layout.t` where each layout node is
 * accompanied by metrics about its shape. These metrics may
 * be used, e.g., to generate SVG elements to decorate nodes.
 *
 * Each node shape is defined as a vertically stacked list of
 * boxes, left-aligned except for possibly the first.
 * The offset of the head box of a layout node from the
 * left-alignment axis of the tail boxes depends on the layout
 * nodes that come prior in a pre-order traversal of the overall
 * layout. For example, the layout node
 * (
 *   x + 1
 * )
 * has metrics `[{h: 1, w: 1}, {h: 1, w: 7}, {h: 1, w: 1}]` but
 * its head box offset varies with its context, e.g., the offset
 * would be `0` if the node is isolated as depicted above, or it
 * would be `8` if embedded in the layout below:
 * let y = (
 *   x + 1
 * ) in _
 */
[@deriving sexp]
type t('annot) = {
  layout: t'('annot),
  metrics: list(box),
}
and t'('annot) =
  | Linebreak
  | Text(string)
  | Align(t('annot))
  | Cat(t('annot), t('annot))
  | Annot('annot, t('annot));

/**
 * A measured layout coupled with the offset of its head box
 */
type with_offset('annot) = (int, t('annot));

let height: t(_) => int;
let width: (~offset: int=?, t(_)) => int;

/**
 * A standard fold operator except that the client is given
 * direct control over the recursion whenever it encounters
 * an `Annot` node, allowing pruning of irrelevant parts of
 * the tree as needed for performance.
 */
let fold:
  (
    ~linebreak: 'acc,
    ~text: string => 'acc,
    ~align: 'acc => 'acc,
    ~cat: ('acc, 'acc) => 'acc,
    // let client control recursion based on annotation
    ~annot: (t('annot) => 'acc, 'annot, t('annot)) => 'acc,
    t('annot)
  ) =>
  'acc;

/**
 * Same as `fold` but additionally exposes the `MeasuredPosition.t`
 * marking the start of each node
 */
let pos_fold:
  (
    ~linebreak: MeasuredPosition.t => 'acc,
    ~text: (MeasuredPosition.t, string) => 'acc,
    ~align: (MeasuredPosition.t, 'acc) => 'acc,
    ~cat: (MeasuredPosition.t, 'acc, 'acc) => 'acc,
    // let client control recursion based on annotation
    ~annot: (
              ~go: t('annot) => 'acc,
              ~indent: int,
              ~start: MeasuredPosition.t,
              'annot,
              t('annot)
            ) =>
            'acc,
    ~indent: int=?,
    ~start: MeasuredPosition.t=?,
    t('annot)
  ) =>
  'acc;

/**
 * `next_position(~indent, start, m)` returns the position at the
 * end of `m` assuming its starting position is `start` and `m` is
 * indented by `indent`.
 */
let next_position:
  (~indent: int, MeasuredPosition.t, t(_)) => MeasuredPosition.t;

module Make: (MemoTbl.S) => {
                              let mk: Layout.t('annot) => t('annot);
                            };
