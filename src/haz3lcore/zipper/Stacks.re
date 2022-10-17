type t'('shard) = {
  // obligations from the left
  l: Stack.t('shard),
  // obligations from both left and right
  m: Stack.t('shard),
  // obligations from the right
  r: Stack.t('shard),
};
type t = t'(Shard.Form.t);

let empty = Stack.{l: empty, m: empty, r: empty};
let is_empty = (==)(empty);

let map = (f: 'a => 'b, bp: t'('a)): t'('b) => {
  l: List.map(f, bp.l),
  m: List.map(f, bp.m),
  r: List.map(f, bp.r),
};

let push_from_sib = (d: Direction.t, t: Tile.t, bp: t): t => {
  let toks = Tile.complete(Direction.toggle(d), t);
  switch (d) {
  | Left => {...bp, l: toks @ bp.l}
  | Right => {...bp, r: toks @ bp.r}
  };
};

let push_from_pre = (ts: list(Tile.t), bp: t) =>
  List.fold_left(Fun.flip(push_from_sib(Left)), bp, ts);
let push_from_suf = (ts: list(Tile.t), bp: t) =>
  List.fold_right(push_from_sib(Right), ts, bp);

let push_from_anc = (a: Ancestor.t, bp: t): t => {
  ...bp,
  m: Ancestor.complete(a),
};
