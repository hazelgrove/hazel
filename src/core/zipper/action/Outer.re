open Util;
open OptUtil.Syntax;
open Zipper;

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

let construct = (from: Direction.t, label: Label.t, z: t): IdGen.t(t) => {
  IdGen.Syntax.(
    switch (label) {
    | [content] when Form.is_whitespace(content) =>
      let+ id = IdGen.fresh;
      Effect.s_touch([id]);
      z
      |> update_siblings(((l, r)) => (l @ [Whitespace({id, content})], r));
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

let unselect_and_zip = (z: t): Segment.t => z |> unselect |> zip;
