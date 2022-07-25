open Sexplib.Std;
open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type caret =
  | Outer
  | Inner(int, int);

// assuming single backpack, shards may appear in selection, backpack, or siblings
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  selection: Selection.t,
  backpack: Backpack.t,
  relatives: Relatives.t,
  caret,
  caret_col_target: int,
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
  let* state = IdGen.get;
  let ls_relatives =
    Relatives.remold(z.relatives)
    |> List.map(rs => Relatives.regrout(d, rs, state))
    |> List.sort(((rel, _), (rel', _)) => {
         open Relatives;
         let c = Int.compare(sort_rank(rel), sort_rank(rel'));
         c != 0 ? c : Int.compare(shape_rank(rel), shape_rank(rel'));
       });
  assert(ls_relatives != []);
  let (relatives, state) = List.hd(ls_relatives);
  let+ () = IdGen.put(state);
  {...z, relatives};
};

module Outer = {
  let unselect = (z: t): t => {
    let relatives =
      z.relatives
      |> Relatives.prepend(z.selection.focus, z.selection.content)
      |> Relatives.reassemble;
    let selection = Selection.clear(z.selection);
    {...z, selection, relatives};
  };

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
      |> Segment.trim_grout_around_whitespace(Left)
      |> Segment.trim_grout_around_whitespace(Right)
      |> Selection.mk(selected.focus);
    let backpack = Backpack.push(selection, z.backpack);
    {...z, backpack};
  };

  let destruct = (z: t): t => {
    let (selected, z) = update_selection(Selection.empty, z);
    let (to_pick_up, to_remove) =
      Segment.incomplete_tiles(selected.content)
      |> List.partition(t =>
           Siblings.contains_matching(t, z.relatives.siblings)
           || Ancestors.parent_matches(t, z.relatives.ancestors)
         );
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
    IncompleteBidelim.set(popped.content);
    {...z, backpack} |> put_selection(popped) |> unselect;
  };

  let construct = (from: Direction.t, label: Label.t, z: t): IdGen.t(t) => {
    IdGen.Syntax.(
      switch (label) {
      | [content] when Form.is_whitespace(content) =>
        let+ id = IdGen.fresh;
        z
        |> update_siblings(((l, r)) =>
             (l @ [Whitespace({id, content})], r)
           );
      | _ =>
        let z = destruct(z);
        let molds = Molds.get(label);
        assert(molds != []);
        // initial mold to typecheck, will be remolded
        let mold = List.hd(molds);
        let+ id = IdGen.fresh;
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
};

let unselect_and_zip = (z: t): Segment.t => z |> Outer.unselect |> zip;
