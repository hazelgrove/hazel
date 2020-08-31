[@deriving sexp]
type box = {
  height: int,
  width: int,
};

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

let height: t(_) => int;
let width: (~offset: int=?, t(_)) => int;

let fold:
  (
    ~linebreak: 'acc,
    ~text: string => 'acc,
    ~align: 'acc => 'acc,
    ~cat: ('acc, 'acc) => 'acc,
    // allow client to control recursion based on annotation
    ~annot: (t('annot) => 'acc, 'annot, t('annot)) => 'acc,
    t('annot)
  ) =>
  'acc;

let pos_fold:
  (
    ~linebreak: MeasuredPosition.t => 'acc,
    ~text: (MeasuredPosition.t, string) => 'acc,
    ~align: (MeasuredPosition.t, 'acc) => 'acc,
    ~cat: (MeasuredPosition.t, 'acc, 'acc) => 'acc,
    // let client control recursion based on annotation
    ~annot: (t('annot) => 'acc, MeasuredPosition.t, 'annot, t('annot)) =>
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

module Make: (MemoTbl.S) => {let mk: Layout.t('annot) => t('annot);};
