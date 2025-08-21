open Util;
open OptUtil.Syntax;
include ZipperBase;

let init: unit => t =
  () => {
    selection: Selection.mk([]),
    relatives: {
      siblings: (
        [],
        [
          Grout({
            id: Id.mk(),
            shape: Convex,
          }),
        ],
      ),
      ancestors: [],
    },
    caret: Outer,
  };

let next_blank = _ => Id.mk();

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type chunkiness =
  | ByChar
  | ByToken;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type planar =
  | Up
  | Down
  | Left(chunkiness)
  | Right(chunkiness);

let from_plane: planar => Direction.t =
  fun
  | Left(_) => Left
  | Right(_) => Right
  | Up => Left
  | Down => Right;

let update_caret = (f: Caret.t => Caret.t, z: t): t => {
  ...z,
  caret: f(z.caret),
};
let set_caret = (caret: Caret.t): (t => t) => update_caret(_ => caret);

let delete_parent = (z: t): t => {
  ...z,
  relatives: Relatives.delete_parent(z.relatives),
};

let zip = (z: t): Segment.t =>
  Relatives.zip(~sel=z.selection.content, z.relatives);

let unzip = (seg: Segment.t): t => {
  selection: Selection.mk([]),
  relatives: {
    siblings: (seg, []),
    ancestors: [],
  },
  caret: Outer,
};

let left_neighbor_monotile: Siblings.t => option(Token.t) =
  s => s |> Siblings.left_neighbor |> OptUtil.and_then(Piece.monotile);

let right_neighbor_monotile: Siblings.t => option(Token.t) =
  s => s |> Siblings.right_neighbor |> OptUtil.and_then(Piece.monotile);

let neighbor_monotiles: Siblings.t => (option(Token.t), option(Token.t)) =
  s => (left_neighbor_monotile(s), right_neighbor_monotile(s));

let regrout = (d: Direction.t, z: t): t => {
  assert(Selection.is_empty(z.selection));
  let relatives = Relatives.regrout(d, z.relatives);
  {
    ...z,
    relatives,
  };
};

let remold = (z: t): t => {
  assert(Selection.is_empty(z.selection));
  {
    ...z,
    relatives: Relatives.remold(z.relatives),
  };
};

let remold_regrout = (d: Direction.t, z: t): t => z |> remold |> regrout(d);

let clear_unparsed_buffer = (z: t): t =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => {
      ...z,
      selection: Selection.empty,
    }
  | _ => z
  };

let unselect = (~erase_buffer=false, z: t): t => {
  /* NOTE(andrew): Erase buffer flag only applies to unparsed buffer,
   * that is, the buffer style that just contains a single flat token.
   * Erasing a buffer that contains arbitrary tiles would be more complex
   * as we can't just empty the selection without regrouting */
  let z = erase_buffer ? clear_unparsed_buffer(z) : z;
  let relatives =
    z.relatives
    |> Relatives.prepend(z.selection.focus, z.selection.content)
    |> Relatives.reassemble;
  let selection = Selection.empty;
  {
    ...z,
    selection,
    relatives,
  };
};
let unselect_and_zip = (~erase_buffer=false, z: t): Segment.t =>
  z |> unselect(~erase_buffer) |> zip;

let replace_selection = (focus, segment, z: t): t => {
  ...z,
  selection: Selection.mk(~focus, segment),
};

let update_selection_and_unselect = (selection: Selection.t, z: t): t =>
  unselect({
    ...z,
    selection,
  });

let grow_selection = (z: t): option(t) => {
  let+ (p, relatives) = Relatives.pop(z.selection.focus, z.relatives);
  let selection = Selection.push(p, z.selection);
  {
    ...z,
    selection,
    relatives,
  };
};

// toggles focus and grows if selection is empty
let shrink_selection = (z: t): option(t) => {
  switch (Selection.pop(z.selection)) {
  | None =>
    let selection = Selection.toggle_focus(z.selection);
    grow_selection({
      ...z,
      selection,
    });
  | Some((p, selection)) =>
    let relatives =
      z.relatives
      |> Relatives.push(selection.focus, p)
      |> Relatives.reassemble;
    Some({
      ...z,
      selection,
      relatives,
    });
  };
};

let toggle_focus = (z: t): t => {
  ...z,
  selection: Selection.toggle_focus(z.selection),
};

let set_focus = (z: t, d: Direction.t): t => {
  let selection = {
    ...z.selection,
    focus: d,
  };
  {
    ...z,
    selection,
  };
};

let directional_unselect = (d: Direction.t, z: t): t => {
  let selection = {
    ...z.selection,
    focus: Direction.toggle(d),
  };
  unselect({
    ...z,
    selection,
  });
};

let unselect = (z: t): t =>
  z.selection.content == [] ? z : directional_unselect(z.selection.focus, z);

let move = (d: Direction.t, z: t): option(t) =>
  if (Selection.is_empty(z.selection)) {
    let+ (p, relatives) = Relatives.pop(d, z.relatives);
    let relatives =
      relatives
      |> Relatives.push(Direction.toggle(d), p)
      |> Relatives.reassemble;
    {
      ...z,
      relatives,
    };
  } else {
    Some(directional_unselect(d, z));
  };

let select = (d: Direction.t, z: t): option(t) =>
  d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

let destruct = (z: t): t =>
  update_selection_and_unselect(Selection.empty, z);

let adj_pos = (d: Direction.t, z: t): t =>
  switch (d) {
  | Left => z
  | Right =>
    switch (move(Left, z)) {
    | None => z
    | Some(z) => z
    }
  };

let put_down_core = (seg: Segment.t, z: t): t =>
  z |> destruct |> replace_selection(Right, seg) |> unselect;

let put_down_seg = (d: Direction.t, seg: Segment.t, z: t): t =>
  z |> put_down_core(seg) |> adj_pos(d);

let local_backpack = (z: t): list(Tile.t) =>
  Relatives.local_missing_shards(z.relatives);

let backpack_hd = (z: t): option(Tile.t) =>
  z |> local_backpack |> ListUtil.hd_opt;

let backpack_find = (tok: Token.t, z: t): option(Tile.t) =>
  if (Form.is_ambiguous_polymorph(tok)) {
    /* Special case for ambiguous polymorphs. These tokens
       occur both on their own as infix ops and as delimiters of
       multi-delimiter forms. To give the singleton form a chance, we
       only match these to incomplete tiles to form their multi forms
       when they're on the top of the stack */
    backpack_hd(z) |> Option.map(Tile.effective_label) == Some([tok])
      ? backpack_hd(z) : None;
  } else {
    List.find_map(
      t => Tile.effective_label(t) == [tok] ? Some(t) : None,
      local_backpack(z),
    );
  };

let put_down_tok = (d: Direction.t, tok: Token.t, z: t): option(t) => {
  /* Does not regrout/remold on its own. */
  let+ target = backpack_find(tok, z);
  put_down_seg(d, [Tile(target)], z);
};

let put_down = (d: Direction.t, z: t): option(t) => {
  /* Does not regrout/remold on its own. */
  let+ target = backpack_hd(z);
  put_down_seg(d, [Tile(target)], z);
};

let will_barf = (tok: Token.t, z: t): bool =>
  put_down_tok(Right, tok, z) != None;

let can_put_down = z =>
  switch (local_backpack(z)) {
  | [] => false
  | _ => z.caret == Outer
  };

let remold_regrout_prev = (z: t): t =>
  switch (move(Left, z)) {
  | None => z
  | Some(z_left) =>
    let z_left = z_left |> remold |> regrout(Right);
    switch (move(Right, z_left)) {
    | None => failwith("Zipper.remold_regrout_prev: move fail")
    | Some(z_right) => z_right
    };
  };

let put_down_regrout_target = (d: Direction.t, target: Tile.t, z: t): t => {
  let z = put_down_core([Tile(target)], z);
  let z = z |> regrout(Left) |> remold;
  let z = remold_regrout_prev(z);
  adj_pos(d, z);
};

let put_down_regrout_remold = (d: Direction.t, z: t): option(t) => {
  let+ target = backpack_hd(z);
  put_down_regrout_target(d, target, z);
};

let put_down_regrout_remold_tok =
    (d: Direction.t, tok: Token.t, z: t): option(t) => {
  let+ target = backpack_find(tok, z);
  put_down_regrout_target(d, target, z);
};

let rec construct =
        (~caret: Direction.t, ~backpack: Direction.t, label: Label.t, z: t): t => {
  switch (label) {
  | [t] when Form.is_string_delim(t) =>
    /* Special case for constructing string literals.
       See Insert.move_into_if_stringlit for more special-casing. */
    construct(~caret, ~backpack, [Form.string_delim ++ Form.string_delim], z)
  | [content] when Form.is_comment(content) =>
    /* Special case for comments, can't rely on the last branch to construct */
    let content = Secondary.construct_comment(content);
    let id = Id.mk();
    let z = destruct(z);
    put_down_seg(caret, Base.mk_secondary(id, content), z);
  | [content] when Form.is_secondary(content) =>
    let content = Secondary.Whitespace(content);
    let id = Id.mk();
    z
    |> update_siblings(((l, r)) =>
         (
           l
           @ [
             Secondary({
               id,
               content,
             }),
           ],
           r,
         )
       );
  | _ =>
    let z = destruct(z);
    let molds = Molds.get(label);
    assert(molds != []);
    // initial mold to typecheck, will be remolded
    let mold = List.hd(molds);
    let id = Id.mk();
    let selections =
      Tile.split_shards(id, label, mold, List.mapi((i, _) => i, label))
      |> List.map(Segment.of_tile)
      |> ListUtil.rev_if(backpack == Right);
    put_down_seg(caret, List.hd(selections), z);
  };
};

let construct_mono = (d: Direction.t, t: Token.t, z: t): t =>
  construct(~caret=d, ~backpack=Left, [t], z);

let rec get_leaf_pieces =
        (syntaxNode: Piece.t, ~ignored_labels: list(list(string)))
        : list(Piece.t) =>
  switch (syntaxNode) {
  | Tile(tile) =>
    /* Check if this tile's label is in the ignored labels */
    let should_ignore =
      List.exists(label => label == tile.label, ignored_labels);
    if (should_ignore) {
      [];
        /* Ignore this tile */
    } else if (tile.children == []) {
      [
        /* It's a leaf piece */
        Tile(tile),
      ];
    } else {
      /* Recurse into the children */
      tile.children
      |> List.concat_map(segment =>
           segment |> List.concat_map(get_leaf_pieces(~ignored_labels))
         );
    };
  | _ => []
  };

// let remove_projector = (id: Id.t, syntax: Piece.t) =>
//   switch (syntax) {
//   | Projector(pr) when pr.id == id =>
//     // just get the label, found as first leaf piece
//     get_leaf_pieces(pr.syntax, ~ignored_labels=[[","]]) |> List.hd
//   | x => x
//   };

let delete = (d: Direction.t, z: t): option(t) => {
  let to_delete = z |> select(d);
  switch (to_delete) {
  | Some({selection: {content: [Projector(_)], _}, _}) =>
    switch () {
    // p.kind
    // TODO(Matt): restore livelit backspace to unproject
    // | Livelit =>
    //   Some(ZipperBase.MapPiece.fast_local(remove_projector(p.id), p.id, z))

    | _ => to_delete |> Option.map(destruct)
    }
  | _ => to_delete |> Option.map(destruct)
  };
};

let replace =
    (~caret: Direction.t, ~backpack: Direction.t, l: Label.t, z: t)
    : option(t) =>
  /* i.e. select and construct, overwriting the selection */
  z |> delete(caret) |> Option.map(construct(~caret, ~backpack, l));

let match_prev = (z: t) =>
  switch (neighbor_monotiles(z.relatives.siblings)) {
  | (Some(t), _) when will_barf(t, z) =>
    switch (delete(Left, z)) {
    | Some(z) => put_down_regrout_remold_tok(Left, t, z)
    | None => Some(z)
    }
  | _ => None
  };

let replace_mono = (d: Direction.t, t: Token.t, z: t): option(t) =>
  replace(~caret=d, ~backpack=Left, [t], z);

let representative_piece = (z: t): option((Piece.t, Direction.t)) => {
  /* The piece to the left of the caret, or if none exists, the piece to the right */
  switch (Siblings.neighbors(sibs_with_sel(z))) {
  | (Some(l), _) => Some((l, Left))
  | (_, Some(r)) => Some((r, Right))
  | _ => None
  };
};

let caret_direction = (z: t): option(Direction.t) =>
  /* Direction the caret is facing in */
  switch (z.caret) {
  | Inner(_) => None
  | Outer =>
    switch (Siblings.neighbors(sibs_with_sel(z))) {
    | (Some(l), Some(r))
        when
          Piece.is_secondary(l)
          && Piece.is_secondary(r)
          && Selection.is_empty(z.selection) =>
      None
    | _ => Siblings.direction_between(sibs_with_sel(z))
    }
  };

let base_point = (measured: Measured.t, z: t): Point.t => {
  switch (representative_piece(z)) {
  | Some((p, d)) =>
    let seg = Piece.disassemble(p);
    switch (d) {
    | Left =>
      let p = ListUtil.last(seg);
      let m = Measured.find_p(~msg="base_point", p, measured);
      m.last;
    | Right =>
      let p = List.hd(seg);
      let m = Measured.find_p(~msg="base_point", p, measured);
      m.origin;
    };
  | None => {
      row: 0,
      col: 0,
    }
  };
};
let caret_point = (measured, z: t): Point.t => {
  let Point.{row, col} = base_point(measured, z);
  {
    row,
    col: col + Caret.offset(z.caret),
  };
};

let selection_anchor_point = (measured, z: t): option(Point.t) => {
  switch (z.selection) {
  | {content: [], _} => None
  | {content, focus: Right, _} =>
    Some(
      Measured.find_p(
        ~msg="selection_anchor_point",
        List.hd(content),
        measured,
      ).
        origin,
    )
  | {content, focus: Left, _} =>
    Some(
      Measured.find_p(
        ~msg="selection_anchor_point",
        ListUtil.last(content),
        measured,
      ).
        last,
    )
  };
};

let serialize = (z: t): string => {
  sexp_of_t(z) |> Sexplib.Sexp.to_string;
};

let to_sexp = sexp_of_t;

let deserialize = (data: string): t => {
  Sexplib.Sexp.of_string(data) |> t_of_sexp;
};

let set_buffer = (z: t, ~mode: Selection.buffer, ~content: Segment.t): t => {
  ...z,
  selection: Selection.mk_buffer(mode, content),
};

let is_linebreak_to_right_of_caret =
    ({relatives: {siblings: (_, r), _}, _}: t): bool => {
  switch (r) {
  | [Secondary(s), ..._] when Secondary.is_linebreak(s) => true
  | _ => false
  };
};

/* Try to complete the syntax to give better semantic feeback.
 * This is a best-effort approach focussed on adding new definitions
 * as opposed to restructuring; it does not complete the syntax in
 * all cases.
 *
 * NOTE: Setting the caret to outer was necessary to 'get it past'
 * string literals, i.e. offer live feeback when typing inside a
 * string; not sure if this is a hack or not, it may be compensating
 * for the put_down logic not working right with string lits. To test,
 * try to look at live evaluation while typing inside a string lit with
 * stuff left to drop in backpack with below set: Outer disabled. */
let try_to_dump_backpack = (zipper: t) => {
  switch (local_backpack(zipper)) {
  | [] => zipper
  | _ =>
    let zipper = {
      ...zipper,
      caret: Outer,
    };
    let rec move_until_cant_put_down = (z_last, z: t) =>
      if (can_put_down(z) && !is_linebreak_to_right_of_caret(z)) {
        switch (move(Right, z)) {
        | None => z
        | Some(z_new) => move_until_cant_put_down(z, z_new)
        };
      } else {
        z_last;
      };
    let rec move_until_can_put_down = (z: t) =>
      if (!can_put_down(z)) {
        switch (move(Right, z)) {
        | None => z
        | Some(z_new) => move_until_can_put_down(z_new)
        };
      } else {
        z;
      };
    let rec go = (z: t): t => {
      let z_can = can_put_down(z) ? z : move_until_can_put_down(z);
      let z_cant = move_until_cant_put_down(z_can, z_can);
      switch (put_down_regrout_remold(Right, z_cant)) {
      | None => z_cant
      | Some(z) => go(z)
      };
    };
    go(zipper);
  };
};

let smart_seg = (~dump_backpack: bool, ~erase_buffer: bool, z: t): Segment.t => {
  let z = erase_buffer ? clear_unparsed_buffer(z) : z;
  let z = dump_backpack ? try_to_dump_backpack(z) : z;
  unselect_and_zip(~erase_buffer, z);
};

let seg_without_buffer =
  smart_seg(~erase_buffer=true, ~dump_backpack=false, _);
