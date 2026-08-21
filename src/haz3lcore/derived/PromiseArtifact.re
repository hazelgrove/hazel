open Language;

/* THE UNIFIED PROMISE ARTIFACT.

   The display is a PROJECTION of a single material object: the
   segment the user is typing, completed syntactically AND reified
   for the type-forced tuple shape they are about to fill in. There
   is no reconstruction — every delimiter, comma, hole and witness
   token the display shows is a REAL piece of this one artifact, with
   the SAME id semantics ran on.

   artifact = reify(obligations) applied to the syntactically
   completed segment. Both are deterministic and caret-free:
     completed = CanonicalCompletion.complete_segment_deep(seg)
     reified   = TypeObligations.reify(obligations, completed)
   This is the EXACT construction CachedStatics.init runs before its
   second statics pass (MakeTerm.from_zip_for_sem_spliced does
   complete_segment_deep then splice=reify on the same erased-buffer
   segment). So the artifact's ids COINCIDE with the analyzed program's
   ids by determinism — reify mints its commas/holes with Id.next
   chains seeded from the site tile, identical on both sides. An
   inspector or error landing on a presumed hole finds it in the
   info_map by construction, with no coordination.

   Per-leaf PROVENANCE is derived exactly as the diff-based chip
   placement does: a shard is USER material iff present in its tile's
   shard_record mask (or its tile is untouched), a grout/piece iff its
   id is in the input segment. A witness leaf (a shard whose typed
   prefix the user supplied) carries typed_len from the record's
   prefix mask — the display renders the prefix normal and the
   remainder ghost. */

type t = {
  /* the input segment (user's real material, buffer erased) */
  seg: Segment.t,
  /* syntactic completion of `seg` (real closers/keywords/witnesses) */
  completed: Segment.t,
  shard_records: list(CanonicalCompletion.shard_record),
  /* completed + reify(obligations): the one material the display
     projects. Superset of `completed`: reify only splices real comma
     tiles + convex holes into deficient sites' children. */
  reified: Segment.t,
  /* ids present in the input segment (deep) — the provenance oracle */
  orig_ids: Hashtbl.t(Id.t, unit),
};

let collect_ids = (seg: Segment.t): Hashtbl.t(Id.t, unit) => {
  let tbl = Hashtbl.create(64);
  let rec go = (sg: Segment.t) =>
    List.iter(
      (p: Piece.t) => {
        Hashtbl.replace(tbl, Piece.id(p), ());
        switch (p) {
        | Tile(t) => List.iter(go, t.children)
        | _ => ()
        };
      },
      sg,
    );
  go(seg);
  tbl;
};

let mk = (~obligations: list(TypeObligations.t), seg: Segment.t): t => {
  let result = CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
  let reified = TypeObligations.reify(obligations, result.completed_seg);
  {
    seg,
    completed: result.completed_seg,
    shard_records: result.shard_records,
    reified,
    orig_ids: collect_ids(seg),
  };
};

let record_of = (art: t, tid: Id.t): option(CanonicalCompletion.shard_record) =>
  List.find_opt(
    (r: CanonicalCompletion.shard_record) => Id.equal(r.tile_id, tid),
    art.shard_records,
  );

/* is a leaf USER material (present as-typed) vs SYSTEM material
   (synthesized by completion/reification)? */
let shard_is_user = (art: t, t: Tile.t, i: int): bool =>
  if (!Hashtbl.mem(art.orig_ids, t.id)) {
    false; /* whole tile synthesized (a reified comma) */
  } else {
    switch (record_of(art, t.id)) {
    | Some(r) => List.mem(i, r.original_shards)
    | None => true /* untouched tile: every shard is the user's */
    };
  };

let piece_is_user = (art: t, p: Piece.t): bool =>
  Hashtbl.mem(art.orig_ids, Piece.id(p));

/* the shard_prefix record for a witness shard (typed-prefix length +
   the id of the user's partial token that the completed shard
   absorbed); None = fully synthesized, no witness */
let prefix_of =
    (art: t, tid: Id.t, i: int): option(IdTagged.IdTag.shard_prefix) =>
  switch (record_of(art, tid)) {
  | Some(r) =>
    List.find_opt(
      (sp: IdTagged.IdTag.shard_prefix) => sp.shard == i,
      r.prefixes,
    )
  | None => None
  };

/* the typed-prefix length of a witness shard (the user typed this
   many chars of the delimiter); None = fully synthesized */
let typed_len_of = (art: t, tid: Id.t, i: int): option(int) =>
  prefix_of(art, tid, i)
  |> Option.map((sp: IdTagged.IdTag.shard_prefix) => sp.len);

/* find a tile by id anywhere in the reified artifact */
let find_reified = (art: t, id: Id.t): option(Tile.t) => {
  let rec go = (sg: Segment.t): option(Tile.t) =>
    List.fold_left(
      (acc, p: Piece.t) =>
        switch (acc, p) {
        | (Some(_), _) => acc
        | (None, Tile(t)) =>
          Id.equal(t.id, id)
            ? Some(t)
            : List.fold_left(
                (a, c) => a == None ? go(c) : a,
                None,
                t.children,
              )
        | (None, _) => None
        },
      None,
      sg,
    );
  go(art.reified);
};

/* the reified separator pieces (real comma tiles + their following
   convex holes) inside a deficient site's last child that are SYSTEM
   material — the promise's owed commas, with the deterministic ids
   statics analyzed. Returned in material order. */
let site_owed_pieces = (art: t, site: Id.t): list(Piece.t) =>
  switch (find_reified(art, site)) {
  | Some(t) =>
    switch (Util.ListUtil.split_last_opt(t.children)) {
    | Some((_, last)) =>
      last
      |> List.filter((p: Piece.t) => !piece_is_user(art, p))
      |> List.filter((p: Piece.t) =>
           switch (p) {
           | Tile({label: [","], _}) => true
           | Grout({shape: Convex, _}) => true
           | _ => false
           }
         )
    | None => []
    }
  | None => []
  };
