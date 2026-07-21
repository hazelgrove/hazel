open Util;

exception Empty_shard_affix;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type step = int;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  [@equal (_, _) => true]
  id: Id.t,
  form: FormId.t,
  shards: (list(int), list(int)),
  children: (list(Segment.t), list(Segment.t)),
};

let label = (a: t): Label.t => Form.label_of(a.form);
let mold = (a: t): Mold.t => Form.mold_of(a.form);

// TODO(d) revisit naming w.r.t. outer vs inner shards
let l_shard = a =>
  ListUtil.hd_opt(fst(a.shards)) |> OptUtil.get_or_raise(Empty_shard_affix);
let r_shard = a =>
  ListUtil.last_opt(snd(a.shards))
  |> OptUtil.get_or_raise(Empty_shard_affix);

let nibs = (a: t) => {
  let (l, _) = Mold.nibs(~index=l_shard(a), mold(a));
  let (_, r) = Mold.nibs(~index=r_shard(a), mold(a));
  (l, r);
};

let zip = (child: Segment.t, {id, form, shards, children}: t): Tile.t => {
  id,
  form,
  shards: fst(shards) @ snd(shards),
  children: fst(children) @ [child, ...snd(children)],
};

let sort = (a: t): Sort.t => {
  let (pre, _suf) = a.shards;
  switch (ListUtil.split_last_opt(pre)) {
  | Some((_, i)) =>
    let (_, l) = Mold.nibs(~index=i, mold(a));
    /* Use the right nib of the last left shard: this is the
     * sort of the child immediately after the caret's left
     * boundary. Correct even when shards are missing between
     * the left and right boundaries (e.g. partial let...in
     * without =), where checking both nibs would disagree
     * and previously fell back to Any. */
    l.sort;
  | None => raise(Empty_shard_affix)
  };
};

let disassemble =
    ({id, form, shards, children: (kids_l, kids_r)}: t): Siblings.t => {
  let (shards_l, shards_r) =
    shards
    |> TupleUtil.map2(Tile.split_shards(id, form))
    |> TupleUtil.map2(List.map(Tile.to_piece));
  let flatten = (shards, kids) =>
    Aba.mk(shards, kids) |> Aba.join(p => [p], Fun.id) |> List.flatten;
  (flatten(shards_l, kids_l), flatten(shards_r, kids_r));
};

let missing_middle_shards = (a: t): list(Tile.t) => {
  let (shards_l, shards_r) = a.shards;
  let last_l =
    ListUtil.last_opt(shards_l) |> OptUtil.get_or_raise(Empty_shard_affix);
  let first_r =
    ListUtil.hd_opt(shards_r) |> OptUtil.get_or_raise(Empty_shard_affix);
  let ls = List.init(first_r - last_l - 1, i => last_l + i + 1);
  Tile.split_shards(a.id, a.form, ls);
};

let reassemble = (match_l: Aba.t(Tile.t, Segment.t) as 'm, match_r: 'm): t => {
  // TODO(d) bit hacky, need to do a flip/orientation pass
  // let match_l = Aba.map_b(Segment.rev, match_l);
  let (t_l, t_r) = Tile.(reassemble(match_l), reassemble(match_r));
  assert(t_l.id == t_r.id);
  {
    id: t_l.id,
    form: t_l.form,
    shards: (t_l.shards, t_r.shards),
    children: (t_l.children, t_r.children),
  };
};
