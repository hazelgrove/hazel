open Util;
include Base;

exception Ambiguous_molds;
exception Invalid_mold;
exception Empty_tile;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = tile;

let id = (t: t) => t.id;

let label = (t: t): Label.t => Form.label_of(t.form);
let mold = (t: t): Mold.t => Form.mold_of(t.form, t.sort);
let has_label = (t: t, lbl: Label.t): bool => label(t) == lbl;
let arity = (t: t): int => List.length(label(t));
let token = (t: t, i: int): Token.t => List.nth(label(t), i);

/* Form predicates. A family bundles label + shape-role, so most of
 * the historical label-comparison predicates are now plain form
 * equality (each label below is spelled by exactly one family);
 * is_paren_shaped keeps the historical label semantics — the
 * ["(",")"] label is shared by the Parens and Ap families. */

let is_form = (t: t, fam: Form.family): bool => t.form == Form.Compound(fam);

let has_label_of = (t: t, fam: Form.family): bool =>
  Form.has_label_of(t.form, fam);

let is_comma = (t: t): bool => t.form == Form.Compound(Comma);

let is_case_rule = (t: t): bool => t.form == Form.Compound(Rule);

let is_case = (t: t): bool => t.form == Form.Compound(Case);

let is_semi = (t: t): bool => t.form == Form.Compound(CellJoin);

let is_dot = (t: t): bool => t.form == Form.Compound(Dot);

let is_tuple_label_eq = (t: t): bool =>
  t.form == Form.Compound(TupleLabeled);

/* parens AND aps: all forms spelling ["(",")"], deliberately */
let is_paren_shaped = (t: t): bool =>
  t.form == Form.Compound(Parens) || t.form == Form.Compound(Ap);

let is_bracket_shaped = (t: t): bool => t.form == Form.Compound(ListLit);

let is_empty_tuple_shaped = (t: t): bool =>
  t.form == Form.Compound(ApEmpty) || t.form == Form.Tok(Token.empty_tuple);

let is_explicit_hole = (t: t): bool =>
  t.form == Form.Tok(Token.explicit_hole);

let is_multidelimiter = (t: t): bool => List.length(label(t)) > 1;

/* let/type/module/theorem/filter forms */
let ends_with_in = (t: t): bool =>
  switch (label(t) |> List.rev) {
  | ["in", ..._] => true
  | _ => false
  };

let is_complete = (t: t) => arity(t) == List.length(t.shards);

let l_shard = t =>
  OptUtil.get_or_raise(Empty_tile, ListUtil.hd_opt(t.shards));
let r_shard = t =>
  OptUtil.get_or_raise(Empty_tile, ListUtil.last_opt(t.shards));

let has_end = (d: Direction.t, t) =>
  switch (d) {
  | Left => l_shard(t) == 0
  | Right => r_shard(t) == arity(t) - 1
  };

let nibs = (t: t) => {
  let (l, _) = Mold.nibs(~index=l_shard(t), mold(t));
  let (_, r) = Mold.nibs(~index=r_shard(t), mold(t));
  (l, r);
};

let shapes = (t: t) => {
  let (l, r) = nibs(t);
  (l.shape, r.shape);
};

let to_piece = t => Tile(t);

let sorted_children = (t: t) => {
  let mold = mold(t);
  Aba.mk(t.shards, t.children)
  |> Aba.aba_triples
  |> List.map(((l, child, r)) => {
       let (_, l) = Mold.nibs(~index=l, mold);
       let (r, _) = Mold.nibs(~index=r, mold);
       (l.sort == r.sort ? l.sort : Any, child);
     });
};

let contained_children = (t: t): list((t, Base.segment, t)) =>
  Aba.mk(t.shards, t.children)
  |> Aba.aba_triples
  |> List.map(((l, child, r)) => {
       let l = {
         ...t,
         shards: [l],
         children: [],
       };
       let r = {
         ...t,
         shards: [r],
         children: [],
       };
       (l, child, r);
     });

let shard_of = (t: t, i: int): t => {
  ...t,
  shards: [i],
  children: [],
};

let split_shards = (id, form, sort, shards) =>
  shards
  |> List.map(i =>
       {
         id,
         form,
         sort,
         shards: [i],
         children: [],
       }
     );

let left_missing_shards = (t: t): list(t) =>
  List.init(l_shard(t), Fun.id) |> split_shards(t.id, t.form, t.sort);

let right_missing_shards = (t: t): list(t) =>
  List.init(arity(t) - r_shard(t) - 1, i => r_shard(t) + i + 1)
  |> split_shards(t.id, t.form, t.sort);

let missing_shards = (t: t): list(t) =>
  List.filter(i => !List.mem(i, t.shards), List.init(arity(t), Fun.id))
  |> split_shards(t.id, t.form, t.sort);

let effective_label = (t: t): list(string) =>
  List.map(List.nth(label(t)), t.shards);

// postcond: output segment is nonempty
let disassemble = ({id, form, sort, shards, children}: t): segment => {
  let shards = split_shards(id, form, sort, shards);
  Aba.mk(shards, children)
  |> Aba.join(s => [to_piece(s)], Fun.id)
  |> List.concat;
};

let disintegrate = ({id, form, sort, shards, _}: t): list(tile) => {
  split_shards(id, form, sort, shards);
};

let reassemble = (match: Aba.t(t, segment)): t => {
  let t = Aba.hd(match);
  let (shards, children) =
    match
    |> Aba.fold_right(
         (t, child, (shards, children)) =>
           (t.shards @ shards, t.children @ [child, ...children]),
         t => (t.shards, t.children),
       );
  // check lengths
  let _ = Aba.mk(shards, children);
  assert(List.sort(Int.compare, shards) == shards);
  {
    id: t.id,
    // discards forms/sorts on non-hd tiles; if they differ (pending
    // remold), the reassembled tile must be remolded
    form: t.form,
    sort: t.sort,
    shards,
    children,
  };
};

let pop_l = (tile: t): (piece, segment) =>
  disassemble(tile)
  |> ListUtil.split_first_opt
  |> OptUtil.get_or_raise(Empty_tile);
let pop_r = (tile: t): (segment, piece) =>
  disassemble(tile)
  |> ListUtil.split_last_opt
  |> OptUtil.get_or_raise(Empty_tile);
