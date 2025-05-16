open Util;

module Caret = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
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
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('p) = {
  selection: Selection.t('p),
  backpack: Backpack.t('p),
  relatives: Relatives.t('p),
  caret: Caret.t,
};

let update_relatives =
    (f: Relatives.t('p) => Relatives.t('p), z: t('p)): t('p) => {
  ...z,
  relatives: f(z.relatives),
};

let update_siblings: (Siblings.t('p) => Siblings.t('p), t('p)) => t('p) =
  f =>
    update_relatives(rs =>
      {
        ...rs,
        siblings: f(rs.siblings),
      }
    );

let put_siblings = (siblings, z: t('p)): t('p) =>
  update_siblings(_ => siblings, z);

let put_selection_content = (content: Segment.t('p), z: t('p)): t('p) => {
  ...z,
  selection: {
    ...z.selection,
    content,
  },
};

let parent = (z: t('p)): option(Piece.t('p)) =>
  Relatives.parent(~sel=z.selection.content, z.relatives);

let sibs_with_sel =
    (
      {
        selection: {content, focus, _},
        relatives: {siblings: (l_sibs, r_sibs), _},
        _,
      }:
        t('p),
    )
    : Siblings.t('p) =>
  switch (focus) {
  | Left => (l_sibs, content @ r_sibs)
  | Right => (l_sibs @ content, r_sibs)
  };

module MapPiece = {
  type updater('p) = Piece.t('p) => Segment.t('p);

  let rec of_segment = (f: updater('p), seg: Segment.t('p)): Segment.t('p) => {
    seg |> List.concat_map(p => f(p)) |> List.map(of_piece(f));
  }
  and of_piece = (f: updater('p), piece: Piece.t('p)): Piece.t('p) => {
    switch (piece) {
    | Tile(t) => Tile(of_tile(f, t))
    | Grout(_)
    | Projector(_)
    | Secondary(_) => piece
    };
  }
  and of_tile = (f: updater('p), t: Tile.t('p)): Tile.t('p) => {
    {
      ...t,
      children: List.map(of_segment(f), t.children),
    };
  };

  let of_siblings = (f: updater('p), sibs: Siblings.t('p)): Siblings.t('p) => (
    of_segment(f, fst(sibs)),
    of_segment(f, snd(sibs)),
  );

  let of_ancestor =
      (f: updater('p), ancestor: Ancestor.t('p)): Ancestor.t('p) => {
    {
      ...ancestor,
      children: (
        List.map(of_segment(f), fst(ancestor.children)),
        List.map(of_segment(f), snd(ancestor.children)),
      ),
    };
  };

  let of_generation =
      (f: updater('p), generation: Ancestors.generation('p))
      : Ancestors.generation('p) => (
    of_ancestor(f, fst(generation)),
    of_siblings(f, snd(generation)),
  );

  let of_ancestors =
      (f: updater('p), ancestors: Ancestors.t('p)): Ancestors.t('p) =>
    List.map(of_generation(f), ancestors);

  let of_selection =
      (f: updater('p), selection: Selection.t('p)): Selection.t('p) => {
    {
      ...selection,
      content: of_segment(f, selection.content),
    };
  };

  /* Maps the updater over all pieces in the zipper
   * (that are not currently unzipped) */
  let go = (f: updater('p), z: t('p)): t('p) => {
    ...z,
    selection: of_selection(f, z.selection),
    relatives: {
      ancestors: of_ancestors(f, z.relatives.ancestors),
      siblings: of_siblings(f, z.relatives.siblings),
    },
  };

  let sib_has_id = (get, z: t('p), id: Id.t): bool => {
    switch (z.relatives.siblings |> get) {
    | Some(l) => Piece.id(l) == id
    | _ => false
    };
  };

  let left_sib_has_id = sib_has_id(Siblings.left_neighbor, _);

  let right_sib_has_id = sib_has_id(Siblings.right_neighbor, _);

  let update_left_sib = (f: Piece.t('p) => Segment.t('p), z: t('p)) => {
    let (l, r) = z.relatives.siblings;
    let sibs = (List.concat_map(f, l), List.concat_map(f, r));
    put_siblings(sibs, z);
  };

  let update_right_sib = (f: Piece.t('p) => Segment.t('p), z: t('p)) => {
    let sibs =
      switch (z.relatives.siblings) {
      | (l, [hd, ...tl]) => (l, f(hd) @ tl)
      | sibs => sibs
      };
    put_siblings(sibs, z);
  };

  let fast_local_seg =
      (f: Piece.t('p) => Segment.t('p), id: Id.t, z: t('p)): t('p) =>
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

  let fast_local =
      (f: Piece.t('p) => Piece.t('p), id: Id.t, z: t('p)): t('p) =>
    fast_local_seg(p => [f(p)], id, z);
};

module FindPiece = {
  let rec in_segment =
          (f: Piece.t('p) => bool, seg: Segment.t('p))
          : option(Piece.t('p)) =>
    switch (seg) {
    | [] => None
    | [hd, ...tl] =>
      if (f(hd)) {
        Some(hd);
      } else {
        in_segment(f, tl);
      }
    }
  and in_piece =
      (f: Piece.t('p) => bool, piece: Piece.t('p)): option(Piece.t('p)) =>
    switch (piece) {
    | Tile(t) => in_tile(f, t)
    | Grout(_)
    | Secondary(_)
    | Projector(_) =>
      if (f(piece)) {
        Some(piece);
      } else {
        None;
      }
    }

  and in_tile =
      (f: Piece.t('p) => bool, t: Tile.t('p)): option(Piece.t('p)) =>
    List.find_map(in_segment(f), t.children);

  let in_siblings =
      (f: Piece.t('p) => bool, sibs: Siblings.t('p)): option(Piece.t('p)) =>
    switch (sibs) {
    | (l, r) =>
      switch (in_segment(f, l)) {
      | Some(p) => Some(p)
      | None => in_segment(f, r)
      }
    };

  let in_ancestor =
      (f: Piece.t('p) => bool, ancestor: Ancestor.t('p))
      : option(Piece.t('p)) =>
    switch (ancestor.children) {
    | (l, r) =>
      switch (List.find_map(in_segment(f), l)) {
      | Some(p) => Some(p)
      | None => List.find_map(in_segment(f), r)
      }
    };

  let in_generation =
      (f: Piece.t('p) => bool, generation: Ancestors.generation('p))
      : option(Piece.t('p)) =>
    switch (generation) {
    | (ancestor, siblings) =>
      switch (in_ancestor(f, ancestor)) {
      | Some(p) => Some(p)
      | None => in_siblings(f, siblings)
      }
    };

  let in_ancestors =
      (f: Piece.t('p) => bool, ancestors: Ancestors.t('p))
      : option(Piece.t('p)) =>
    List.find_map(in_generation(f), ancestors);

  let in_selection =
      (f: Piece.t('p) => bool, selection: Selection.t('p))
      : option(Piece.t('p)) =>
    switch (selection.content) {
    | [] => None
    | [hd, ...tl] =>
      if (f(hd)) {
        Some(hd);
      } else {
        in_segment(f, tl);
      }
    };

  let in_zipper = (f: Piece.t('p) => bool, z: t('p)): option(Piece.t('p)) => {
    switch (in_selection(f, z.selection)) {
    | Some(p) => Some(p)
    | None =>
      switch (in_ancestors(f, z.relatives.ancestors)) {
      | Some(p) => Some(p)
      | None => in_siblings(f, z.relatives.siblings)
      }
    };
  };
};

// let remove_all_projectors = (z: t('p)): t('p) =>
//   MapPiece.go(
//     fun
//     | Projector(pr) => Piece.unparenthesize(pr.syntax)
//     | x => [x],
//     z,
//   );
