open Util;

module Caret = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Outer
    | Inner(int, int);

  let decrement: t => t =
    fun
    | Outer
    | Inner(_, 0) => Outer
    | Inner(d, c) => Inner(d, c - 1);

  let offset: t => int =
    fun
    | Outer => 0
    | Inner(_, c) => c + 1;
};

// assuming single backpack, shards may appear in selection, backpack, or siblings
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  selection: Selection.t,
  backpack: Backpack.t,
  relatives: Relatives.t,
  caret: Caret.t,
};

let update_relatives = (f: Relatives.t => Relatives.t, z: t): t => {
  ...z,
  relatives: f(z.relatives),
};

let update_siblings: (Siblings.t => Siblings.t, t) => t =
  f => update_relatives(rs => {...rs, siblings: f(rs.siblings)});

let put_siblings = (siblings, z: t): t => update_siblings(_ => siblings, z);

let put_selection_content = (content: Segment.t, z): t => {
  ...z,
  selection: {
    ...z.selection,
    content,
  },
};

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

let focal_segment =
    (
      {
        selection: {content, _},
        relatives: {siblings: (l_sibs, r_sibs), _},
        _,
      }: t,
    )
    : Segment.t =>
  l_sibs @ content @ r_sibs;

module MapSegment = {
  type updater = Segment.t => Segment.t;

  let rec of_segment = (f: updater, seg: Segment.t): Segment.t => {
    seg |> List.map(of_piece(f)) |> f;
  }
  and of_piece = (f: updater, piece: Piece.t): Piece.t => {
    switch (piece) {
    | Tile(t) => Tile(of_tile(f, t))
    | Grout(_)
    | Projector(_)
    | Secondary(_) => piece
    };
  }
  and of_tile = (f: updater, t: Tile.t): Tile.t => {
    {...t, children: List.map(of_segment(f), t.children)};
  };

  let of_siblings = (f: updater, sibs: Siblings.t): Siblings.t => {
    (of_segment(f, fst(sibs)), of_segment(f, snd(sibs)));
  };

  let of_ancestor = (f: updater, ancestor: Ancestor.t): Ancestor.t => {
    {
      ...ancestor,
      children: (
        List.map(of_segment(f), fst(ancestor.children)),
        List.map(of_segment(f), snd(ancestor.children)),
      ),
    };
  };

  let of_generation =
      (f: updater, generation: Ancestors.generation): Ancestors.generation => (
    of_ancestor(f, fst(generation)),
    of_siblings(f, snd(generation)),
  );

  let of_ancestors = (f: updater, ancestors: Ancestors.t): Ancestors.t =>
    List.map(of_generation(f), ancestors);

  let of_selection = (f: updater, selection: Selection.t): Selection.t => {
    {...selection, content: of_segment(f, selection.content)};
  };

  let go = (f: updater, z: t): t => {
    ...z,
    selection: of_selection(f, z.selection),
    relatives: {
      ancestors: of_ancestors(f, z.relatives.ancestors),
      siblings: of_siblings(f, z.relatives.siblings),
    },
  };

  let fast_local = (f: updater, id: Id.t, z: t): t =>
    //TODO(andrew): cleanup, doc
    //TODO(andrew): does fast path actually work?
    if (List.exists(p => Piece.id(p) == id, z.relatives.siblings |> fst)
        || List.exists(p => Piece.id(p) == id, z.relatives.siblings |> snd)) {
      let (l, r) = z.relatives.siblings;
      //this doesn't work as siblings may have incomplete term (or just whitespace)
      // which will crash skel
      let sibs = (f(l), f(r));
      put_siblings(sibs, z);
    } else {
      go(f, z);
    };
};

module MapPiece = {
  /* Maps the updater over all pieces in the zipper
   * (that are not currently unzipped) */

  let go = (f: Piece.t => Piece.t, z: t): t => {
    ...z,
    selection: MapSegment.of_selection(List.map(f), z.selection),
    relatives: {
      ancestors: MapSegment.of_ancestors(List.map(f), z.relatives.ancestors),
      siblings: MapSegment.of_siblings(List.map(f), z.relatives.siblings),
    },
  };

  let sib_has_id = (get, z: t, id: Id.t): bool => {
    switch (z.relatives.siblings |> get) {
    | Some(l) => Piece.id(l) == id
    | _ => false
    };
  };

  let update_left_sib = (f: Piece.t => Piece.t, z: t) => {
    let (l, r) = z.relatives.siblings;
    let sibs = (List.map(f, l), List.map(f, r));
    put_siblings(sibs, z);
  };

  let update_right_sib = (f: Piece.t => Piece.t, z: t) => {
    let sibs =
      switch (z.relatives.siblings) {
      | (l, [hd, ...tl]) => (l, [f(hd), ...tl])
      | sibs => sibs
      };
    put_siblings(sibs, z);
  };

  let fast_local = (f: Piece.t => Piece.t, id: Id.t, z: t): t =>
    /* This applies the function to the piece in the zipper having id id, and
     * then replaces the id of the resulting piece with the idea of the old
     * piece, ensuring that the root id remains stable. This function assumes
     * the cursor is not inside the piece to be updated. This is optimized to
     * be O(1) when the piece is directly to the left or right of the cursor,
     * otherwise it is O(|zipper|) */
    if (sib_has_id(Siblings.left_neighbor, z, id)) {
      update_left_sib(f, z);
    } else if (sib_has_id(Siblings.right_neighbor, z, id)) {
      update_right_sib(f, z);
    } else {
      go(f, z);
    };
};
