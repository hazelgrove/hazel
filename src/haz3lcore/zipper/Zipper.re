open Sexplib.Std;
open Util;
open OptUtil.Syntax;

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
  // col_target: int,
};

let init: int => t =
  id => {
    selection: {
      focus: Left,
      content: [],
    },
    backpack: [],
    relatives: {
      siblings: ([], [Grout({id, shape: Convex})]),
      ancestors: [],
    },
    caret: Outer,
    // col_target: 0,
  };

let next_blank = id => {
  (id + 1, init(id));
};

[@deriving (show({with_path: false}), sexp, yojson)]
type state = (t, IdGen.state);

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

let update_relatives = (f: Relatives.t => Relatives.t, z: t): t => {
  ...z,
  relatives: f(z.relatives),
};

let update_siblings: (Siblings.t => Siblings.t, t) => t =
  f => update_relatives(rs => {...rs, siblings: f(rs.siblings)});

let parent = (z: t): option(Piece.t) =>
  Relatives.parent(~sel=z.selection.content, z.relatives);

let zip = (z: t): Segment.t =>
  Relatives.zip(~sel=z.selection.content, z.relatives);

let sibs_with_sel =
    (
      {
        selection: {content, focus},
        relatives: {siblings: (l_sibs, r_sibs), _},
        _,
      }: t,
    )
    : Siblings.t =>
  switch (focus) {
  | Left => (l_sibs, content @ r_sibs)
  | Right => (l_sibs @ content, r_sibs)
  };

let pop_backpack = (z: t) =>
  Backpack.pop(Relatives.local_incomplete_tiles(z.relatives), z.backpack);

let neighbor_monotiles: Siblings.t => (option(Token.t), option(Token.t)) =
  siblings =>
    switch (Siblings.neighbors(siblings)) {
    | (Some(l), Some(r)) => (Piece.monotile(l), Piece.monotile(r))
    | (Some(l), None) => (Piece.monotile(l), None)
    | (None, Some(r)) => (None, Piece.monotile(r))
    | (None, None) => (None, None)
    };

let remold_regrout = (d: Direction.t, z: t): IdGen.t(t) => {
  assert(Selection.is_empty(z.selection));
  open IdGen.Syntax;
  let+ relatives = Relatives.regrout(d, Relatives.remold(z.relatives));
  {...z, relatives};
};

let unselect = (z: t): t => {
  let relatives =
    z.relatives
    |> Relatives.prepend(z.selection.focus, z.selection.content)
    |> Relatives.reassemble;
  let selection = Selection.clear(z.selection);
  {...z, selection, relatives};
};
let unselect_and_zip = (z: t): Segment.t => z |> unselect |> zip;

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

let directional_unselect = (d: Direction.t, z: t): t => {
  let selection = {...z.selection, focus: Direction.toggle(d)};
  unselect({...z, selection});
};

let move = (d: Direction.t, z: t): option(t) =>
  if (Selection.is_empty(z.selection)) {
    // let balanced = !Backpack.is_balanced(z.backpack);
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
    |> Selection.mk(selected.focus);
  Segment.tiles(selection.content)
  |> List.map((t: Tile.t) => t.id)
  |> Effect.s_touch;
  let backpack = Backpack.push(selection, z.backpack);
  {...z, backpack};
};

let destruct = (~destroy_kids=true, z: t): t => {
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
    z.backpack
    |> Backpack.remove_matching(to_remove)
    |> Backpack.push_s(
         to_pick_up
         |> List.map(Segment.of_tile)
         |> List.map(Selection.mk(z.selection.focus)),
       );
  {...z, backpack};
};

let directional_destruct = (d: Direction.t, z: t): option(t) =>
  z |> select(d) |> Option.map(destruct);

let put_down = (z: t): option(t) => {
  let z = destruct(z);
  let+ (_, popped, backpack) = pop_backpack(z);
  Segment.tiles(popped.content)
  |> List.map((t: Tile.t) => t.id)
  |> Effect.s_touch;
  {...z, backpack} |> put_selection(popped) |> unselect;
};

let rec construct = (from: Direction.t, label: Label.t, z: t): IdGen.t(t) => {
  IdGen.Syntax.(
    switch (label) {
    | [t] when Form.is_string_delim(t) =>
      /* Special case for constructing string literals.
         See Insert.move_into_if_stringlit for more special-casing. */
      construct(Left, [Form.string_delim ++ Form.string_delim], z)
    | [content] when Form.is_comment(content) =>
      /* Special case for comments, can't rely on the last branch to construct */
      let content = Secondary.construct_comment(content);
      let+ id = IdGen.fresh;
      Effect.s_touch([id]);
      let z = destruct(z);
      let selections = [Selection.mk(from, Base.mk_secondary(id, content))];
      let backpack = Backpack.push_s(selections, z.backpack);
      Option.get(put_down({...z, backpack}));

    | [content] when Form.is_secondary(content) =>
      let content = Secondary.Whitespace(content);
      let+ id = IdGen.fresh;
      Effect.s_touch([id]);
      z
      |> update_siblings(((l, r)) => (l @ [Secondary({id, content})], r));
    | _ =>
      let z = destruct(z);
      let molds = Molds.get(label);
      assert(molds != []);
      // initial mold to typecheck, will be remolded
      let mold = List.hd(molds);
      let+ id = IdGen.fresh;
      Effect.s_touch([id]);
      let selections =
        Tile.split_shards(id, label, mold, List.mapi((i, _) => i, label))
        |> List.map(Segment.of_tile)
        |> List.map(Selection.mk(from))
        |> ListUtil.rev_if(from == Right);
      let backpack = Backpack.push_s(selections, z.backpack);
      Option.get(put_down({...z, backpack}));
    }
  );
};

let replace =
    (d: Direction.t, l: Label.t, (z, id_gen): state): option(state) =>
  /* i.e. select and construct, overwriting the selection */
  z |> select(d) |> Option.map(z => construct(d, l, z, id_gen));

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

let base_point = (measured: Measured.t, z: t): Measured.Point.t => {
  switch (representative_piece(z)) {
  | Some((p, d)) =>
    let seg = Piece.disassemble(p);
    switch (d) {
    | Left =>
      let p = ListUtil.last(seg);
      let m = Measured.find_p(p, measured);
      m.last;
    | Right =>
      let p = List.hd(seg);
      let m = Measured.find_p(p, measured);
      m.origin;
    };
  | None => {row: 0, col: 0}
  };
};
let caret_point = (measured, z: t): Measured.Point.t => {
  let Measured.Point.{row, col} = base_point(measured, z);
  {row, col: col + Caret.offset(z.caret)};
};

let serialize = (z: t): string => {
  sexp_of_t(z) |> Sexplib.Sexp.to_string;
};

let deserialize = (data: string): t => {
  Sexplib.Sexp.of_string(data) |> t_of_sexp;
};
