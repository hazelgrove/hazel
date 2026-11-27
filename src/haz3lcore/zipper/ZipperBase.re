open Util;

include CaretBase;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type caret = CaretBase.t;

/* Refractor state extracted to Refractors.re - see state location docs there */
module Refractor = Refractors;

// shards may appear in the selection or siblings; "missing" shards
// (the derived missing-shard set) are those an incomplete tile still lacks
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  selection: Selection.t,
  relatives: Relatives.t,
  caret,
  /* Like projectors but not replacing syntax */
  refractors: Refractor.t,
};

let update_refractors = (z: t, f: Refractor.t => Refractor.t): t => {
  ...z,
  refractors: f(z.refractors),
};

let update_manuals = (f, z: t): t => {
  ...z,
  refractors: {
    ...z.refractors,
    manuals: f(z.refractors.manuals),
  },
};

let add_manual = (~model=?, id: Id.t, kind: ProjectorCore.Kind.t, z: t): t =>
  /* Drop any existing entry for this id first: `manuals` is consumed both
   * as an assoc list (first match wins) and via Id.Map.of_list (last
   * wins), so a duplicate makes logic and rendering disagree. */
  update_manuals(
    x =>
      [
        (id, Refractors.mk_entry(~model?, kind)),
        ...List.filter(((id', _)) => id' != id, x),
      ],
    z,
  );

let update_ephemerals = (f, z: t): t => {
  ...z,
  refractors: {
    ...z.refractors,
    multis: {
      ...z.refractors.multis,
      ephemerals: f(z.refractors.multis.ephemerals),
    },
  },
};

/* same text, selection, caret and manual probes. probe passes rebuild
   the record on every calculate, so identity alone says little */
let same_content = (a: t, b: t): bool =>
  a === b
  || a.relatives === b.relatives
  && a.selection === b.selection
  && a.caret == b.caret
  && a.refractors.manuals === b.refractors.manuals;

let update_suppressed = (f, z: t): t => {
  ...z,
  refractors: {
    ...z.refractors,
    multis: {
      ...z.refractors.multis,
      suppressed: f(z.refractors.multis.suppressed),
    },
  },
};

let update_refractor =
    (
      id: Id.t,
      f: option(Refractors.entry) => option(Refractors.entry),
      z: t,
    )
    : t =>
  z
  |> update_manuals(map => ListUtil.assoc_update(id, f, map))
  |> update_ephemerals(map => Id.Map.update(id, f, map));

let update_relatives = (f: Relatives.t => Relatives.t, z: t): t => {
  ...z,
  relatives: f(z.relatives),
};

let update_siblings: (Siblings.t => Siblings.t, t) => t =
  f =>
    update_relatives(rs =>
      {
        ...rs,
        siblings: f(rs.siblings),
      }
    );

let put_siblings = (siblings, z: t): t => update_siblings(_ => siblings, z);

let parent = (z: t): option(Piece.t) =>
  Relatives.parent(~sel=z.selection.content, z.relatives);

let sibs_with_sel =
    (
      {
        selection: {content, focus, _},
        relatives: {siblings: (l_sibs, r_sibs), _},
        _,
      }: t,
    )
    : Siblings.t =>
  switch (focus) {
  | Left => (l_sibs, content @ r_sibs)
  | Right => (l_sibs @ content, r_sibs)
  };

module MapPiece = {
  type updater = Piece.t => Segment.t;

  /* Identity-preserving: a piece, segment or tile with nothing changed
     beneath it comes back as the same object. Incremental layers key on
     piece identity -- MakeTerm.Incr reuses an item only if its pieces are
     the same objects (Segment.ptr_eq), Measured's cache likewise -- and
     rebuilding every record re-minted the whole program: a livelit's
     commit (ProjectorPerform.update, through fast_local_seg's general
     case) re-parsed every item, about 30% of a Kids' Choice slider
     release in Firefox. */
  let rec of_segment = (f: updater, seg: Segment.t): Segment.t => {
    let changed = ref(false);
    let out =
      List.concat_map(
        p => {
          let ps = List.map(of_piece(f), f(p));
          switch (ps) {
          | [q] when q === p => ()
          | _ => changed := true
          };
          ps;
        },
        seg,
      );
    changed^ ? out : seg;
  }
  and of_piece = (f: updater, piece: Piece.t): Piece.t => {
    switch (piece) {
    | Tile(t) =>
      let t' = of_tile(f, t);
      t' === t ? piece : Tile(t');
    | Projector(pr) =>
      /* Projector syntax and splice contents are ordinary zipped
       * pieces: recurse so updates reach projectors/pieces nested
       * inside other projectors' splices. */
      let syntax = of_segment(f, pr.syntax);
      syntax === pr.syntax
        ? piece
        : Projector({
            ...pr,
            syntax,
          });
    | Splice(s) =>
      let content = of_segment(f, s.content);
      content === s.content
        ? piece
        : Splice({
            ...s,
            content,
          });
    | Grout(_)
    | Secondary(_) => piece
    };
  }
  and of_tile = (f: updater, t: Tile.t): Tile.t => {
    let children = List.map(of_segment(f), t.children);
    List.for_all2((a, b) => a === b, children, t.children)
      ? t
      : {
        ...t,
        children,
      };
  };

  let of_siblings = (f: updater, sibs: Siblings.t): Siblings.t => (
    of_segment(f, fst(sibs)),
    of_segment(f, snd(sibs)),
  );

  let of_ancestor = (f: updater, ancestor: Ancestor.t): Ancestor.t =>
    switch (ancestor) {
    | Tile(a) =>
      Ancestor.Tile({
        ...a,
        children: (
          List.map(of_segment(f), fst(a.children)),
          List.map(of_segment(f), snd(a.children)),
        ),
      })
    | Projector(a) =>
      Ancestor.Projector({
        ...a,
        before: of_segment(f, a.before),
        after: of_segment(f, a.after),
      })
    | Splice(_) => ancestor
    };

  let of_generation =
      (f: updater, generation: Ancestors.generation): Ancestors.generation => (
    of_ancestor(f, fst(generation)),
    of_siblings(f, snd(generation)),
  );

  let of_ancestors = (f: updater, ancestors: Ancestors.t): Ancestors.t =>
    List.map(of_generation(f), ancestors);

  let of_selection = (f: updater, selection: Selection.t): Selection.t => {
    {
      ...selection,
      content: of_segment(f, selection.content),
    };
  };

  /* Maps the updater over all pieces in the zipper
   * (that are not currently unzipped) */
  let go = (f: updater, z: t): t => {
    ...z,
    selection: of_selection(f, z.selection),
    relatives: {
      ancestors: of_ancestors(f, z.relatives.ancestors),
      siblings: of_siblings(f, z.relatives.siblings),
    },
  };

  let sib_has_id = (get, z: t, id: Id.t): bool => {
    switch (z.relatives.siblings |> get) {
    | Some(l) => Piece.id(l) == id
    | _ => false
    };
  };

  let left_sib_has_id = sib_has_id(Siblings.neighbor(Left));

  let right_sib_has_id = sib_has_id(Siblings.neighbor(Right));

  let update_left_sib = (f: Piece.t => Segment.t, z: t) => {
    let (l, r) = z.relatives.siblings;
    let sibs = (List.concat_map(f, l), List.concat_map(f, r));
    put_siblings(sibs, z);
  };

  let update_right_sib = (f: Piece.t => Segment.t, z: t) => {
    let sibs =
      switch (z.relatives.siblings) {
      | (l, [hd, ...tl]) => (l, f(hd) @ tl)
      | sibs => sibs
      };
    put_siblings(sibs, z);
  };

  /* (^ticket(721)) as [of_segment], but stepping only into the pieces
     [within] says may hold the piece being updated; the others come back as
     the very same objects, as [of_segment] gives them. A livelit's commit
     on the Witch slide walked all 43,171 of its pieces to find one
     projector: 11 to 20 ms of the edit after a slider's step (run
     42826692242) */
  let rec of_segment_within =
          (f: updater, within: Piece.t => bool, seg: Segment.t): Segment.t => {
    let changed = ref(false);
    let out =
      List.concat_map(
        p =>
          if (within(p)) {
            let ps = List.map(of_piece_within(f, within), f(p));
            switch (ps) {
            | [q] when q === p => ()
            | _ => changed := true
            };
            ps;
          } else {
            [p];
          },
        seg,
      );
    changed^ ? out : seg;
  }
  and of_piece_within =
      (f: updater, within: Piece.t => bool, piece: Piece.t): Piece.t =>
    switch (piece) {
    | Tile(t) =>
      let children = List.map(of_segment_within(f, within), t.children);
      List.for_all2((a, b) => a === b, children, t.children)
        ? piece
        : Tile({
            ...t,
            children,
          });
    | Projector(pr) =>
      let syntax = of_segment_within(f, within, pr.syntax);
      syntax === pr.syntax
        ? piece
        : Projector({
            ...pr,
            syntax,
          });
    | Splice(sp) =>
      let content = of_segment_within(f, within, sp.content);
      content === sp.content
        ? piece
        : Splice({
            ...sp,
            content,
          });
    | Grout(_)
    | Secondary(_) => piece
    };

  let go_within = (f: updater, within: Piece.t => bool, z: t): t => {
    let seg = of_segment_within(f, within);
    let sibs = ((l, r): Siblings.t): Siblings.t => (seg(l), seg(r));
    let ancestor = (a: Ancestor.t): Ancestor.t =>
      switch (a) {
      | Tile(a) =>
        Ancestor.Tile({
          ...a,
          children: (
            List.map(seg, fst(a.children)),
            List.map(seg, snd(a.children)),
          ),
        })
      | Projector(a) =>
        Ancestor.Projector({
          ...a,
          before: seg(a.before),
          after: seg(a.after),
        })
      | Splice(_) => a
      };
    {
      ...z,
      selection: {
        ...z.selection,
        content: seg(z.selection.content),
      },
      relatives: {
        ancestors:
          List.map(
            ((a, ss)) => (ancestor(a), sibs(ss)),
            z.relatives.ancestors,
          ),
        siblings: sibs(z.relatives.siblings),
      },
    };
  };

  let fast_local_seg = (f: Piece.t => Segment.t, id: Id.t, z: t): t =>
    /* This applies the function to the piece in the zipper having id id, and
     * then replaces the id of the resulting piece with the idea of the old
     * piece, ensuring that the root id remains stable. This function assumes
     * the cursor is not inside the piece to be updated. This is optimized to
     * be O(1) when the piece is directly to the left or right of the cursor,
     * otherwise it is O(|zipper|) */
    if (left_sib_has_id(z, id)) {
      update_left_sib(f, z);
    } else if (right_sib_has_id(z, id)) {
      update_right_sib(f, z);
    } else {
      go(f, z);
    };

  /* (^ticket(721)) [fast_local_seg] stepping only into the pieces [within]
     says may hold piece [id]; the whole walk if that misses it (a piece
     [within] cannot place, as one inside another projector's splice) */
  let fast_local_seg_within =
      (~within: Piece.t => bool, f: Piece.t => Segment.t, id: Id.t, z: t): t =>
    if (left_sib_has_id(z, id) || right_sib_has_id(z, id)) {
      fast_local_seg(f, id, z);
    } else {
      let hit = ref(false);
      let f' = p => {
        if (Piece.id(p) == id) {
          hit := true;
        };
        f(p);
      };
      let z' = go_within(f', within, z);
      hit^ ? z' : go(f, z);
    };
};

/* Like MapPiece but for segment-level transformations.
   Useful when you need to see/modify sequences of pieces together
   (e.g., linebreak followed by spaces for indentation). */
module MapSegment = {
  type updater = Segment.t => Segment.t;

  let of_segment = (f: updater, seg: Segment.t): Segment.t =>
    Segment.map_deep(f, seg);

  let of_siblings = (f: updater, sibs: Siblings.t): Siblings.t => (
    of_segment(f, fst(sibs)),
    of_segment(f, snd(sibs)),
  );

  /* Tile ancestors only: like Segment.map_deep, this does not reach into
     a projector's syntax or a splice's content, which Indentation also
     leaves to their own frames. */
  let of_ancestor = (f: updater, ancestor: Ancestor.t): Ancestor.t =>
    switch (ancestor) {
    | Tile(a) =>
      Tile({
        ...a,
        children: (
          List.map(of_segment(f), fst(a.children)),
          List.map(of_segment(f), snd(a.children)),
        ),
      })
    | Projector(_)
    | Splice(_) => ancestor
    };

  let of_generation =
      (f: updater, generation: Ancestors.generation): Ancestors.generation => (
    of_ancestor(f, fst(generation)),
    of_siblings(f, snd(generation)),
  );

  let of_ancestors = (f: updater, ancestors: Ancestors.t): Ancestors.t =>
    List.map(of_generation(f), ancestors);

  let of_selection = (f: updater, selection: Selection.t): Selection.t => {
    {
      ...selection,
      content: of_segment(f, selection.content),
    };
  };

  /* Maps the updater over all segments in the zipper */
  let go = (f: updater, z: t): t => {
    ...z,
    selection: of_selection(f, z.selection),
    relatives: {
      ancestors: of_ancestors(f, z.relatives.ancestors),
      siblings: of_siblings(f, z.relatives.siblings),
    },
  };
};
