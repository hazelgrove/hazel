open Util;
open OptUtil.Syntax;

/* Inspector-only information for the hole Tab would insert first. Resolve
   its owner/child slot in the completion that was actually typechecked;
   neither mint new IDs nor typecheck a speculative program here. */
let compute =
    (
      ~root: Sort.t,
      ~syntax: option(CachedSyntax.t)=?,
      ~statics: CachedStatics.t,
      z: Zipper.t,
    )
    : option(Language.Info.t) =>
  if (z.caret != Outer || !Selection.is_empty(z.selection)) {
    None;
  } else {
    let* snapshot = statics.completion;
    /* [syntax]: the frame's segment, which the frame's other completions
       (and usually statics) already read */
    let source =
      switch (syntax) {
      | Some(syntax) => CompletionQuery.engine_seg(~syntax, z)
      | None => MakeTerm.semantic_source(z)
      };
    /* Include IDs: Segment.equal deliberately ignores tile identity.
       compare, unlike (==), skips the pieces two zips share: all but the
       caret's ancestors. */
    if (compare(source, snapshot.source) != 0) {
      None; /* Includes annotation/context edits during the statics debounce. */
    } else {
      let* chip = CompletionQuery.chip_at_caret(~root, ~seg=source, z);
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
let last:
  ref(
    option(
      (
        CachedStatics.t,
        Zipper.t,
        Sort.t,
        option(CachedSyntax.t),
        option(Language.Info.t),
      ),
    ),
  ) =
  ref(None);
let at_caret =
    (
      ~root: Sort.t,
      ~syntax: option(CachedSyntax.t)=?,
      ~statics: CachedStatics.t,
      z: Zipper.t,
    )
    : option(Language.Info.t) => {
  /* the syntax is boxed afresh each call: compare what it holds */
  let same_syntax = (a: option(CachedSyntax.t)) =>
    switch (a, syntax) {
    | (None, None) => true
    | (Some(a), Some(b)) => a === b
    | _ => false
    };
  switch (last^) {
  | Some((s, z', root', syntax', r))
      when s === statics && z' === z && root' == root && same_syntax(syntax') => r
  | _ =>
    let r = compute(~root, ~syntax?, ~statics, z);
    last := Some((statics, z, root, syntax, r));
    r;
  };
};
