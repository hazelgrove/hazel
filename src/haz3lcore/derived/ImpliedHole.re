open Util;
open OptUtil.Syntax;

/* Inspector-only information for the hole Tab would insert first. Resolve
   its owner/child slot in the completion that was actually typechecked;
   neither mint new IDs nor typecheck a speculative program here. */
let compute =
    (~statics: CachedStatics.t, z: Zipper.t): option(Language.Info.t) =>
  if (z.caret != Outer || !Selection.is_empty(z.selection)) {
    None;
  } else {
    let* snapshot = statics.completion;
    let source = MakeTerm.semantic_source(z);
    /* Include IDs: Segment.equal deliberately ignores tile identity. The
       hash includes them too, and the source shares all but its spine
       with the zipper's pieces, which are hashed already: comparing
       structurally walked the whole program on every redraw. */
    if (!(
          Util.ValueHash.same(source, snapshot.source)
          || source == snapshot.source
        )) {
      None; /* Includes annotation/context edits during the statics debounce. */
    } else {
      let* chip = CompletionQuery.chip_at_caret(~seg=source, z);
      switch (chip.delimiters) {
      | [
          {
            leading_hole: true,
            typed_len: None,
            of_shard: Some((id, shard)),
            _,
          },
          ..._,
        ]
          when shard > 0 =>
        let* (_, _, piece) = Segment.find_ctx(snapshot.completed, id);
        switch (piece) {
        | Tile(t) =>
          let* child =
            List.nth_opt(t.children, Tile.child_index_before(t, shard));
          switch (
            List.find_opt(p => !Piece.is_secondary(p), List.rev(child))
          ) {
          | Some(p) =>
            let* info = Id.Map.find_opt(Piece.id(p), statics.info_map);
            switch (info) {
            | InfoExp({user_term: {term: EmptyHole, _}, _})
            | InfoPat({user_term: {term: EmptyHole, _}, _}) => Some(info)
            | _ => None
            };
          | _ => None
          };
        | _ => None
        };
      | _ => None
      };
    };
  };

/* The page asks on every redraw, and a livelit release redraws several
   times with the same zipper and statics (one per message from the eval
   worker): answer those from the last call. */
let last: ref(option((CachedStatics.t, Zipper.t, option(Language.Info.t)))) =
  ref(None);
let at_caret =
    (~statics: CachedStatics.t, z: Zipper.t): option(Language.Info.t) =>
  switch (last^) {
  | Some((s, z', r)) when s === statics && z' === z => r
  | _ =>
    let r = compute(~statics, z);
    last := Some((statics, z, r));
    r;
  };
