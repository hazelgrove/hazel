open Zipper;
open Util;
open OptUtil.Syntax;

let capture = (z): t => z;

let delete = (d: Direction.t, z: t): option(t) => {
  let+ z = select(d, z);
  let z = capture(z);
  destroy_selection(z);
};

/* Unwrap a string/comment/label: delete the token and re-insert
 * its content character-by-character, as if the user had typed it.
 * This is the inverse of selection wrapping for quote delimiters. */
let unwrap_quote = (d: Direction.t, t: Token.t, z: t, ~root): option(t) => {
  let content = String.sub(t, 1, String.length(t) - 2);
  let+ z = delete(d, z);
  if (String.length(content) == 0) {
    z;
  } else {
    let result =
      Token.to_list(content)
      |> List.fold_left(
           (z_opt, c) =>
             switch (z_opt) {
             | None => None
             | Some(z) => Insert.go(c, z, ~root)
             },
           Some(z),
         );
    switch (result) {
    | Some(z) => z
    | None => z
    };
  };
};

let outer = (d: Direction.t, z: t, ~root): option(t) =>
  switch (Zipper.neighbor_token(d, z)) {
  | Some(t) when Token.length(t) > 1 && !Token.is_string_or_comment(t) =>
    Insert.replace_shard(d, Token.rm_edge(d, t), z, ~root)
  | Some(t) when Token.is_string_or_comment(t) =>
    unwrap_quote(d, t, z, ~root)
  | _ => delete(d, z)
  };

let rm_nth_right = (idx, t, z, ~root) =>
  Insert.replace_shard(Right, Token.rm_nth(t, idx), z, ~root);

let inner_left = (idx: int, z: t, ~root): option(t) =>
  switch (Zipper.neighbor_token(Right, z)) {
  | Some(t) when Token.is_string_or_comment(t) && idx == 0 =>
    unwrap_quote(Right, t, z |> Caret.set(Outer), ~root)
  | Some(t) =>
    let z = Caret.set(idx == 0 ? Outer : Inner(idx - 1), z);
    let+ z = rm_nth_right(idx, t, z, ~root);
    Zipper.remold_regrout(Left, z, ~root);
  | None => z |> Caret.set(Outer) |> delete(Right)
  };

let is_last_inner_pos = (t, idx) => Token.length(t) - 2 == idx;

let inner_right = (idx: int, z: t, ~root): option(t) =>
  switch (Zipper.neighbor_token(Right, z)) {
  | Some(t) when Token.is_string_or_comment(t) && is_last_inner_pos(t, idx) =>
    unwrap_quote(Right, t, z |> Caret.set(Outer), ~root)
  | Some(t) =>
    let* z = rm_nth_right(idx + 1, t, z, ~root);
    is_last_inner_pos(t, idx)
      ? z |> Caret.set(Outer) |> Zipper.move(Right) : Some(z);
  | None => z |> Caret.set(Outer) |> delete(Left)
  };

let destruct = (d: Direction.t, z: t, ~root): option(t) =>
  switch (z.caret) {
  | _ when z.selection.content != [] =>
    Some(z |> capture |> destroy_selection)
  | Outer => outer(d, z, ~root)
  | Inner(idx) =>
    switch (d) {
    | Left => inner_left(idx, z, ~root)
    | Right => inner_right(idx, z, ~root)
    }
  };

let go = (d: Direction.t, z: t, ~root): option(t) => {
  switch (Triggers.destruct(z)) {
  | Some(z) => Some(z)
  | None =>
    let+ z = destruct(d, z, ~root);
    /* In the virtual-grout world, regrout is a no-op, but we keep
     * the call shape so the merge_or_noop pass still has a chance
     * to coalesce tokens after deletion. */
    let z =
      z
      |> Insert.merge_or_noop(~root)
      |> remold_regrout(d, ~root)
      |> Insert.merge_or_noop(~root);
    Zipper.rescan_reassemble(d, z, ~root);
  };
};
