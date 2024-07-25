open Util;
open OptUtil.Syntax;
include ZipperBase;

let init: unit => t =
  () => {
    selection: Selection.mk([]),
    backpack: [],
    relatives: {
      siblings: ([], [Grout({id: Id.mk(), shape: Convex})]),
      ancestors: [],
    },
    caret: Outer,
  };

let next_blank = _ => Id.mk();

[@deriving (show({with_path: false}), sexp, yojson)]
type chunkiness =
  | ByChar
  | MonoByChar
  | ByToken;

[@deriving (show({with_path: false}), sexp, yojson)]
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
  backpack: [],
  relatives: {
    siblings: (seg, []),
    ancestors: [],
  },
  caret: Outer,
};

let pop_backpack = (z: t) =>
  Backpack.pop(Relatives.local_incomplete_tiles(z.relatives), z.backpack);

let left_neighbor_monotile: Siblings.t => option(Token.t) =
  s => s |> Siblings.left_neighbor |> OptUtil.and_then(Piece.monotile);

let right_neighbor_monotile: Siblings.t => option(Token.t) =
  s => s |> Siblings.right_neighbor |> OptUtil.and_then(Piece.monotile);

let neighbor_monotiles: Siblings.t => (option(Token.t), option(Token.t)) =
  s => (left_neighbor_monotile(s), right_neighbor_monotile(s));

let regrout = (d: Direction.t, z: t): t => {
  assert(Selection.is_empty(z.selection));
  let relatives = Relatives.regrout(d, z.relatives);
  {...z, relatives};
};

let remold = (z: t): t => {
  assert(Selection.is_empty(z.selection));
  {...z, relatives: Relatives.remold(z.relatives)};
};

let remold_regrout = (d: Direction.t, z: t): t => z |> remold |> regrout(d);

let clear_unparsed_buffer = (z: t) =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => {...z, selection: Selection.empty}
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
  {...z, selection, relatives};
};
let unselect_and_zip = (~erase_buffer=false, z: t): Segment.t =>
  z |> unselect(~erase_buffer) |> zip;

let update_selection = (selection: Selection.t, z: t): (Selection.t, t) => {
  let old = z.selection;
  // used to be necessary to unselect when selection update
  // included remold/regrout, now no longer necessary if needs
  // to be changed but keeping for now to minimize change
  let z = unselect({...z, selection});
  (old, z);
};

let put_selection = (sel: Selection.t, z: t): t =>
  snd(update_selection(sel, z));

let grow_selection = (z: t): option(t) => {
  let+ (p, relatives) = Relatives.pop(z.selection.focus, z.relatives);
  let selection = Selection.push(p, z.selection);
  {...z, selection, relatives};
};

// toggles focus and grows if selection is empty
let shrink_selection = (z: t): option(t) => {
  switch (Selection.pop(z.selection)) {
  | None =>
    let selection = Selection.toggle_focus(z.selection);
    grow_selection({...z, selection});
  | Some((p, selection)) =>
    let relatives =
      z.relatives
      |> Relatives.push(selection.focus, p)
      |> Relatives.reassemble;
    Some({...z, selection, relatives});
  };
};

let toggle_focus = (z: t): t => {
  ...z,
  selection: Selection.toggle_focus(z.selection),
};

let directional_unselect = (d: Direction.t, z: t): t => {
  let selection = {...z.selection, focus: Direction.toggle(d)};
  unselect({...z, selection});
};

let move = (d: Direction.t, z: t): option(t) =>
  if (Selection.is_empty(z.selection)) {
    let+ (p, relatives) = Relatives.pop(d, z.relatives);
    let relatives =
      relatives
      |> Relatives.push(Direction.toggle(d), p)
      |> Relatives.reassemble;
    {...z, relatives};
  } else {
    Some(directional_unselect(d, z));
  };

let select = (d: Direction.t, z: t): option(t) =>
  d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

let pick_up = (z: t): t => {
  let (selected, z) = update_selection(Selection.empty, z);
  let selection =
    selected.content
    |> Segment.trim_grout_around_secondary(Left)
    |> Segment.trim_grout_around_secondary(Right)
    |> Selection.mk;
  Segment.tiles(selection.content)
  |> List.map((t: Tile.t) => t.id)
  |> Effect.s_touch;
  let backpack = Backpack.push(selection, z.backpack);
  {...z, backpack};
};

let destruct = (~destroy_kids=true, z: t): t => {
  let backpack =
    Backpack.remove_uni_tiles_with_deep_matches(z.backpack, z.selection);
  let (selected, z) = update_selection(Selection.empty, z);
  let (to_pick_up, to_remove) =
    Segment.incomplete_tiles(selected.content)
    |> List.partition(t =>
         Siblings.contains_matching(t, z.relatives.siblings)
         || Ancestors.parent_matches(t, z.relatives.ancestors)
       );
  /* If flag is set, break up tiles and remove children */
  let to_pick_up =
    destroy_kids
      ? List.map(Tile.disintegrate, to_pick_up) |> List.flatten : to_pick_up;
  Effect.s_touch(List.map((t: Tile.t) => t.id, to_pick_up));
  let backpack =
    backpack
    |> Backpack.remove_matching(to_remove)
    |> Backpack.push_s(
         to_pick_up |> List.map(Segment.of_tile) |> List.map(Selection.mk),
       );
  {...z, backpack};
};

let delete = (d: Direction.t, z: t): option(t) =>
  z |> select(d) |> Option.map(destruct);

let put_down = (d: Direction.t, z: t): option(t) => {
  let z = destruct(z);
  let* (_, popped, backpack) = pop_backpack(z);
  Segment.tiles(popped.content)
  |> List.map((t: Tile.t) => t.id)
  |> Effect.s_touch;
  let z = {...z, backpack} |> put_selection(popped) |> unselect;
  switch (d) {
  | Left => Some(z)
  | Right => move(Left, z)
  };
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
    Effect.s_touch([id]);
    let z = destruct(z);
    let selections = [Selection.mk(Base.mk_secondary(id, content))];
    let backpack = Backpack.push_s(selections, z.backpack);
    Option.get(put_down(caret, {...z, backpack}));

  | [content] when Form.is_secondary(content) =>
    let content = Secondary.Whitespace(content);
    let id = Id.mk();
    Effect.s_touch([id]);
    z |> update_siblings(((l, r)) => (l @ [Secondary({id, content})], r));
  | _ =>
    let z = destruct(z);
    let molds = Molds.get(label);
    assert(molds != []);
    // initial mold to typecheck, will be remolded
    let mold = List.hd(molds);
    let id = Id.mk();
    Effect.s_touch([id]);
    let selections =
      Tile.split_shards(id, label, mold, List.mapi((i, _) => i, label))
      |> List.map(Segment.of_tile)
      |> List.map(Selection.mk)
      |> ListUtil.rev_if(backpack == Right);
    let backpack = Backpack.push_s(selections, z.backpack);
    Option.get(put_down(caret, {...z, backpack}));
  };
};

let construct_mono = (d: Direction.t, t: Token.t, z: t): t =>
  construct(~caret=d, ~backpack=Left, [t], z);

let replace =
    (~caret: Direction.t, ~backpack: Direction.t, l: Label.t, z: t)
    : option(t) =>
  /* i.e. select and construct, overwriting the selection */
  z |> delete(caret) |> Option.map(construct(~caret, ~backpack, l));

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
    | (Some(l), Some(r)) when Piece.is_secondary(l) && Piece.is_secondary(r) =>
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
  | None => {row: 0, col: 0}
  };
};
let caret_point = (measured, z: t): Point.t => {
  let Point.{row, col} = base_point(measured, z);
  {row, col: col + Caret.offset(z.caret)};
};

let serialize = (z: t): string => {
  sexp_of_t(z) |> Sexplib.Sexp.to_string;
};

let deserialize = (data: string): t => {
  Sexplib.Sexp.of_string(data) |> t_of_sexp;
};

let can_put_down = z =>
  switch (pop_backpack(z)) {
  | Some(_) => z.caret == Outer
  | None => false
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
  switch (zipper.backpack) {
  | [] => zipper
  | _ =>
    let zipper = {...zipper, caret: Outer};
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
    let rec go = (z: t): t =>
      if (can_put_down(z)) {
        let z_can = move_until_cant_put_down(z, z);
        switch (put_down(Right, z_can)) {
        | None => z_can
        | Some(z) =>
          let z = regrout(Right, z);
          go(z);
        };
      } else {
        let z_can = move_until_can_put_down(z);
        let z_can = move_until_cant_put_down(z_can, z_can);
        switch (put_down(Right, z_can)) {
        | None => z_can
        | Some(z) =>
          let z = regrout(Right, z);
          go(z);
        };
      };
    go(zipper);
  };
};

let smart_seg = (~dump_backpack: bool, ~erase_buffer: bool, z: t) => {
  let z = erase_buffer ? clear_unparsed_buffer(z) : z;
  let z = dump_backpack ? try_to_dump_backpack(z) : z;
  unselect_and_zip(~erase_buffer, z);
};

let seg_for_view = smart_seg(~erase_buffer=false, ~dump_backpack=false);
let seg_for_sem = smart_seg(~erase_buffer=true, ~dump_backpack=true);

let seg_without_buffer = smart_seg(~erase_buffer=true, ~dump_backpack=false);
