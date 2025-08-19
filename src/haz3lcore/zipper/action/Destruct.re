open Zipper;
open Util;
open OptUtil.Syntax;

let will_duo_split = t => Token.duosplits(t) != [];

let construct_duosplit = (d: Direction.t, t, z: t): t =>
  Zipper.construct(
    ~d,
    ~id=Insert.adjacent_monotile_or_new_id(d, z),
    ~backpack=d,
    Token.duosplits(t),
    z,
  );

let is_string_or_comment = t => Token.is_string(t) || Token.is_comment(t);

let outer_left = (z: t): option(t) =>
  switch (Zipper.left_neighbor_shard(z)) {
  | Some(t) when is_string_or_comment(t) => delete(Left, z)
  | Some(t) when will_duo_split(t) =>
    let+ z = delete(Left, z);
    construct_duosplit(Left, t, z);
  | Some(t) when Token.length(t) > 1 =>
    Insert.replace_shard(Left, Token.rm_last(t), z)
  | _ => delete(Left, z)
  };

let outer_right = (z: Zipper.t): option(Zipper.t) =>
  switch (Zipper.right_neighbor_shard(z)) {
  | Some(t) when is_string_or_comment(t) => delete(Right, z)
  | Some(t) when will_duo_split(t) =>
    let+ z = delete(Right, z);
    construct_duosplit(Right, t, z);
  | Some(t) when Token.length(t) > 1 =>
    Insert.replace_shard(Right, Token.rm_first(t), z)
  | _ => delete(Right, z)
  };

let rm_nth_right = (idx, t, z) =>
  Insert.replace_shard(Right, Token.rm_nth(idx, t), z);

let is_last_inner_pos = (t, idx) => Token.length(t) - 2 == idx;

let inner_left = (idx: int, z: t): option(t) =>
  switch (Zipper.right_neighbor_shard(z)) {
  | Some(t) when is_string_or_comment(t) && idx == 0 =>
    z |> Caret.set(Outer) |> Zipper.delete(Right)
  | Some(t) when will_duo_split(t) && idx == 0 =>
    let z = Caret.set(Outer, z);
    let+ z = Zipper.delete(Right, z);
    construct_duosplit(Right, t, z);
  | Some(t) =>
    let z = Caret.set(idx == 0 ? Outer : Inner(idx - 1), z);
    let+ z = rm_nth_right(idx, t, z);
    /* From here on handles a weird edge case where we must
       account for grout getting inserted after the caret */
    let z_init = Insert.expand_or_barf_neighbors(z);
    let init_left_nhbr = Siblings.right_neighbor(z_init.relatives.siblings);
    let z = remold_regrout(Left, z_init);
    let new_nhbr = Siblings.right_neighbor(z.relatives.siblings);
    switch (new_nhbr, z.caret, Zipper.move(Right, z)) {
    | (Some(p), Inner(_), Some(z))
        when Piece.is_grout(p) && new_nhbr != init_left_nhbr => z
    | _ => z
    };
  | None => z |> Caret.set(Outer) |> Zipper.delete(Right)
  };

let inner_right = (idx: int, z: t): option(t) =>
  switch (Zipper.right_neighbor_shard(z)) {
  | Some(t) when is_string_or_comment(t) && is_last_inner_pos(t, idx) =>
    z |> Caret.set(Outer) |> Zipper.delete(Right)
  | Some(t) when will_duo_split(t) && is_last_inner_pos(t, idx) =>
    let z = Caret.set(Outer, z);
    let+ z = Zipper.delete(Right, z);
    construct_duosplit(Left, t, z);
  | Some(t) =>
    let* z = rm_nth_right(idx + 1, t, z);
    is_last_inner_pos(t, idx)
      ? z |> Caret.set(Outer) |> Zipper.move(Right) : Some(z);
  | None => z |> Caret.set(Outer) |> Zipper.delete(Left)
  };

let inner = (d: Direction.t, idx: int, z: t): option(t) =>
  switch (d) {
  | Left => inner_left(idx, z)
  | Right => inner_right(idx, z)
  };

let outer = (z: Zipper.t, d: Direction.t): option(Zipper.t) =>
  switch (d) {
  | Left => outer_left(z)
  | Right => outer_right(z)
  };

let destruct = (d: Direction.t, z: t): option(t) =>
  switch (z.caret) {
  | _ when z.selection.content != [] => Some(Zipper.destroy_selection(z))
  | Outer => outer(z, d)
  | Inner(idx) => inner(d, idx, z)
  };

/* If the caret is precisely between two tokens, merge those tokens */
let merge_or_noop = (z: t): t =>
  switch (Zipper.neighbor_shards(z)) {
  | (Some(l), Some(r))
      when Token.is_potential_token(Token.append(l, r)) && z.caret == Outer =>
    /* We remove the left manually, and then replace the right */
    let z = Zipper.delete(Left, z) |> Option.get;
    let z = Insert.replace_shard(Right, Token.append(l, r), z) |> Option.get;
    let z = Caret.set(Inner(Token.length(l) - 1), z);
    /* Regrouting direction needed to merge prefixs into infix eg ! */
    remold_regrout(Right, z);
  | _ => z
  };

let go = (d: Direction.t, z: t): option(t) => {
  let+ z = destruct(d, z);
  z
  |> merge_or_noop
  |> Insert.expand_or_barf_neighbors
  |> remold_regrout(d)
  |> merge_or_noop; /* If grout disappears we may have another merge opportunity */
};
