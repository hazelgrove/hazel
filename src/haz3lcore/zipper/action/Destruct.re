open Zipper;
open Util;
open OptUtil.Syntax;

let capture = (z): t => {
  let junk_id =
    switch (z.selection.content) {
    | [Tile(t)] when List.length(t.label) == 1 => Some(t.id)
    | [Tile(t)]
        when
          List.length(Tile.effective_label(t)) == 1
          && !
               List.exists(
                 (tt: Tile.t) => tt.id == t.id,
                 Relatives.local_missing_shards(z.relatives),
               ) =>
      /* Subtle condition, reliant on the selection being length 1 */
      Some(t.id)
    | [Grout(g)] => Some(g.id)
    | _ => None
    };
  Grout.cache_id(junk_id);
  z;
};

let delete = (d: Direction.t, z: t): option(t) => {
  let+ z = select(d, z);
  let z = capture(z);
  destroy_selection(z);
};

let outer = (d: Direction.t, z: t): option(t) =>
  switch (Zipper.neighbor_shard(d, z)) {
  | Some(t) when Token.length(t) > 1 && !Token.is_string_or_comment(t) =>
    Insert.replace_shard(d, Token.rm_edge(d, t), z)
  | _ => delete(d, z)
  };

let rm_nth_right = (idx, t, z) =>
  Insert.replace_shard(Right, Token.rm_nth(idx, t), z);

let inner_left = (idx: int, z: t): option(t) =>
  switch (Zipper.neighbor_shard(Right, z)) {
  | Some(t) when Token.is_string_or_comment(t) && idx == 0 =>
    z |> Caret.set(Outer) |> delete(Right)
  | Some(t) =>
    let z = Caret.set(idx == 0 ? Outer : Inner(idx - 1), z);
    let+ z_init = rm_nth_right(idx, t, z);
    let z_final = Zipper.remold_regrout(Left, z_init);
    Insert.adjust_caret_pos(~z_final, ~z_init);
  | None => z |> Caret.set(Outer) |> delete(Right)
  };

let is_last_inner_pos = (t, idx) => Token.length(t) - 2 == idx;

let inner_right = (idx: int, z: t): option(t) =>
  switch (Zipper.neighbor_shard(Right, z)) {
  | Some(t) when Token.is_string_or_comment(t) && is_last_inner_pos(t, idx) =>
    z |> Caret.set(Outer) |> delete(Right)
  | Some(t) =>
    let* z = rm_nth_right(idx + 1, t, z);
    is_last_inner_pos(t, idx)
      ? z |> Caret.set(Outer) |> Zipper.move(Right) : Some(z);
  | None => z |> Caret.set(Outer) |> delete(Left)
  };

let destruct = (d: Direction.t, z: t): option(t) =>
  switch (z.caret) {
  | _ when z.selection.content != [] =>
    Some(z |> capture |> destroy_selection)
  | Outer => outer(d, z)
  | Inner(idx) =>
    switch (d) {
    | Left => inner_left(idx, z)
    | Right => inner_right(idx, z)
    }
  };

let go = (d: Direction.t, z: t): option(t) =>
  switch (Triggers.destruct(z)) {
  | Some(z) => Some(z)
  | None =>
    let+ z = destruct(d, z);
    /* If grout disappears we may have a second merge opportunity */
    z |> Insert.merge_or_noop |> remold_regrout(d) |> Insert.merge_or_noop;
  };
