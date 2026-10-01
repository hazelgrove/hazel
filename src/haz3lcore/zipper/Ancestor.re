open Util;

exception Empty_shard_affix;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type step = int;

/* A Tile-shaped ancestor: the caret is inside one of a tile's children. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type tile_anc = {
  [@equal (_, _) => true]
  id: Id.t,
  form: Form.t,
  sort: Sort.t,
  shards: (list(int), list(int)),
  children: (list(Segment.t), list(Segment.t)),
};

/* A Projector-shaped ancestor carries the projector context surrounding
 * the splice the caret is currently in. The projector's full syntax is
 * recovered by splicing the currently-zipped Splice piece between
 * [before] and [after]. Any other pieces of projector.syntax (including
 * sibling splices) live in these two segments. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type proj_anc = {
  [@equal (_, _) => true]
  id: Id.t,
  kind: ProjectorCore.Kind.t,
  model: string,
  /* Carried so that zipping back out of the splice rebuilds the projector
     as it was: without them, a caret in a splice reset a docked projector
     to inline and hid a livelit's shown syntax. */
  [@sexp.default ProjectorCore.Placement.Inline] [@yojson.default
                                                   ProjectorCore.Placement.Inline
                                                 ]
  placement: ProjectorCore.Placement.t,
  [@sexp.default false] [@yojson.default false]
  show_syntax: bool,
  before: Segment.t,
  after: Segment.t,
};

/* A Splice-shaped ancestor: caret is directly inside a splice's content.
 * The parent projector context lives in the next-up generation as a
 * Projector ancestor. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type splice_anc = {
  [@equal (_, _) => true]
  id: Id.t,
  sort: Sort.t,
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Tile(tile_anc)
  | Projector(proj_anc)
  | Splice(splice_anc);

let id: t => Id.t =
  fun
  | Tile(a) => a.id
  | Projector(a) => a.id
  | Splice(a) => a.id;

let is_tile: t => option(tile_anc) =
  fun
  | Tile(a) => Some(a)
  | _ => None;

let is_projector: t => option(proj_anc) =
  fun
  | Projector(a) => Some(a)
  | _ => None;

let is_splice: t => option(splice_anc) =
  fun
  | Splice(a) => Some(a)
  | _ => None;

let label = (a: tile_anc): Label.t => Form.label_of(a.form);
let mold = (a: tile_anc): Mold.t => Form.mold_of(a.form, a.sort);

// TODO(d) revisit naming w.r.t. outer vs inner shards
let l_shard = (a: tile_anc) =>
  ListUtil.hd_opt(fst(a.shards)) |> OptUtil.get_or_raise(Empty_shard_affix);
let r_shard = (a: tile_anc) =>
  ListUtil.last_opt(snd(a.shards))
  |> OptUtil.get_or_raise(Empty_shard_affix);

let tile_nibs = (a: tile_anc) => {
  let (l, _) = Mold.nibs(~index=l_shard(a), mold(a));
  let (_, r) = Mold.nibs(~index=r_shard(a), mold(a));
  (l, r);
};

let nibs = (a: t): (Nib.t, Nib.t) =>
  switch (a) {
  | Tile(a) => tile_nibs(a)
  | Projector(_)
  | Splice(_) =>
    /* Projector and Splice ancestors are opaque Convex/Convex. */
    Nib.(
      {
        shape: Convex,
        sort: Any,
      },
      {
        shape: Convex,
        sort: Any,
      },
    )
  };

/* Zip a child segment back up into the parent piece.
 * - Tile: wrap child in the tile at the caret position.
 * - Splice: produce a Splice piece whose content is the child.
 * - Projector: produce a Projector piece whose syntax is before ++ child ++ after.
 *   (This is used when zipping a Projector frame whose child is the already-
 *   zipped Splice segment; [before]/[after] surround it in projector syntax.) */
let zip = (child: Segment.t, a: t): Base.piece =>
  switch (a) {
  | Tile({id, form, sort, shards, children}) =>
    Base.Tile({
      id,
      form,
      sort,
      shards: fst(shards) @ snd(shards),
      children: fst(children) @ [child, ...snd(children)],
    })
  | Splice({id, _}) => Piece.mk_splice(~id, child)
  | Projector({id, kind, model, placement, show_syntax, before, after}) =>
    Base.Projector(
      ProjectorCore.mk(
        ~id,
        ~placement,
        ~show_syntax,
        kind,
        before @ child @ after,
        model,
      ),
    )
  };

let sort = (a: t): Sort.t =>
  switch (a) {
  | Tile(a) =>
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
  | Projector(_) => Sort.Any
  | Splice({sort, _}) => sort
  };

/* Disassemble returns the siblings this ancestor contributes when its
 * "shards" are broken back into pieces. For tiles this splits shards/kids
 * into two segments. For Projector/Splice ancestors there is nothing to
 * disassemble — the caret can only enter/exit them atomically — so we
 * return empty siblings. */
let disassemble = (a: t): Siblings.t =>
  switch (a) {
  | Tile({id, form, sort, shards, children: (kids_l, kids_r)}) =>
    let (shards_l, shards_r) =
      shards
      |> TupleUtil.map2(Tile.split_shards(id, form, sort))
      |> TupleUtil.map2(List.map(Tile.to_piece));
    let flatten = (shards, kids) =>
      Aba.mk(shards, kids) |> Aba.join(p => [p], Fun.id) |> List.flatten;
    (flatten(shards_l, kids_l), flatten(shards_r, kids_r));
  | Projector(_)
  | Splice(_) => (Segment.empty, Segment.empty)
  };

let missing_middle_shards = (a: t): list(Tile.t) =>
  switch (a) {
  | Tile(a) =>
    let (shards_l, shards_r) = a.shards;
    let last_l =
      ListUtil.last_opt(shards_l) |> OptUtil.get_or_raise(Empty_shard_affix);
    let first_r =
      ListUtil.hd_opt(shards_r) |> OptUtil.get_or_raise(Empty_shard_affix);
    let ls = List.init(first_r - last_l - 1, i => last_l + i + 1);
    Tile.split_shards(a.id, a.form, a.sort, ls);
  | Projector(_)
  | Splice(_) => []
  };

let reassemble = (match_l: Aba.t(Tile.t, Segment.t) as 'm, match_r: 'm): t => {
  let (t_l, t_r) = Tile.(reassemble(match_l), reassemble(match_r));
  assert(t_l.id == t_r.id);
  Tile({
    id: t_l.id,
    form: t_l.form,
    sort: t_l.sort,
    shards: (t_l.shards, t_r.shards),
    children: (t_l.children, t_r.children),
  });
};
