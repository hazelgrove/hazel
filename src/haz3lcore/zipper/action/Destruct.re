open Zipper;
open Util;
open OptUtil.Syntax;

let is_string_or_comment = t => Token.is_string(t) || Token.is_comment(t);

let outer = (d: Direction.t, z: t): option(t) =>
  switch (Zipper.neighbor_shard(d, z)) {
  | Some(t) when Token.length(t) > 1 && !is_string_or_comment(t) =>
    Insert.replace_shard(d, Token.rm_edge(d, t), z)
  | _ => delete(d, z)
  };

let rm_nth_right = (idx, t, z) =>
  Insert.replace_shard(Right, Token.rm_nth(idx, t), z);

let inner_left = (idx: int, z: t): option(t) =>
  switch (Zipper.neighbor_shard(Right, z)) {
  | Some(t) when is_string_or_comment(t) && idx == 0 =>
    z |> Caret.set(Outer) |> Zipper.delete(Right)
  | Some(t) =>
    let z = Caret.set(idx == 0 ? Outer : Inner(idx - 1), z);
    let+ z_init = rm_nth_right(idx, t, z);
    let z_final = remold_regrout(Left, z_init);
    /* Handle an edge case around grout insertion */
    Insert.grout_edge_case(~z_final, ~z_init);
  | None => z |> Caret.set(Outer) |> Zipper.delete(Right)
  };

let is_last_inner_pos = (t, idx) => Token.length(t) - 2 == idx;

let inner_right = (idx: int, z: t): option(t) =>
  switch (Zipper.neighbor_shard(Right, z)) {
  | Some(t) when is_string_or_comment(t) && is_last_inner_pos(t, idx) =>
    z |> Caret.set(Outer) |> Zipper.delete(Right)
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

let destruct = (d: Direction.t, z: t): option(t) =>
  switch (z.caret) {
  | _ when z.selection.content != [] => Some(Zipper.destroy_selection(z))
  | Outer => outer(d, z)
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
  z |> merge_or_noop |> remold_regrout(d) |> merge_or_noop; /* If grout disappears we may have another merge opportunity */
};
