open Zipper;
open Util;
open OptUtil.Syntax;

let destruct = (d: Direction.t, z: t): option(t) => {
  let is_last_inner_pos = (t, idx) => Token.length(t) - 2 == idx;
  let will_duo_split = t => Token.duosplits(t) != [];
  let is_string_or_comment = t => Token.is_string(t) || Token.is_comment(t);
  let delete_right = z =>
    z |> Zipper.Caret.set(Outer) |> Zipper.delete(Right);
  let construct_right = (id, l, z: option(t)) =>
    Option.map(Zipper.construct(~id, ~d=Right, ~backpack=Right, l), z);
  let construct_left = (id, l, z) =>
    Option.map(Zipper.construct(~d=Left, ~id, ~backpack=Left, l), z);
  let left_id =
    switch (adjacent_monotile_id(Left, z)) {
    | Some(id) => id
    | None => Id.mk()
    };
  let right_id =
    switch (adjacent_monotile_id(Right, z)) {
    | Some(id) => id
    | None => Id.mk()
    };
  switch (d, z.caret, Zipper.neighbor_shards(z)) {
  /* When there's a selection, defer to Outer */
  | _ when z.selection.content != [] =>
    z |> Zipper.destroy_selection |> Option.some
  /* Special cases for monotiles which can split into duotiles, e.g. `[]` */
  | (Left, Outer, (Some(t), _)) when will_duo_split(t) =>
    z |> Zipper.delete(Left) |> construct_left(left_id, Token.duosplits(t))
  | (Right, Outer, (_, Some(t))) when will_duo_split(t) =>
    z |> delete_right |> construct_right(right_id, Token.duosplits(t))
  | (Left, Inner(0), (_, Some(t))) when will_duo_split(t) =>
    z |> delete_right |> construct_right(right_id, Token.duosplits(t))
  | (Right, Inner(idx), (_, Some(t)))
      when will_duo_split(t) && is_last_inner_pos(t, idx) =>
    z |> delete_right |> construct_left(right_id, Token.duosplits(t))
  /* Special cases for string literals. When deletion would
     remove an outer quote, we instead remove the whole string */
  | (Left, Outer, (Some(t), _)) when is_string_or_comment(t) =>
    Zipper.delete(Left, z)
  | (Right, Outer, (_, Some(t))) when is_string_or_comment(t) =>
    delete_right(z)
  | (Left, Inner(0), (_, Some(t))) when is_string_or_comment(t) =>
    delete_right(z)
  | (Right, Inner(idx), (_, Some(t)))
      when is_string_or_comment(t) && is_last_inner_pos(t, idx) =>
    delete_right(z)
  /* Unspecial cases */
  | (Left, Inner(idx), (_, Some(t))) =>
    let z = Zipper.Caret.set(idx == 0 ? Outer : Inner(idx - 1), z);
    let+ z = Insert.replace_shard(Right, Token.rm_nth(idx, t), z);
    /* From here on handles a weird edge case where we must
       account for grout getting inserted after the caret */
    let z = Insert.expand_or_barf_neighbors(z);
    let init_left_nhbr = Siblings.right_neighbor(z.relatives.siblings);
    let z = remold_regrout(d, z);
    let new_nhbr = Siblings.right_neighbor(z.relatives.siblings);
    switch (new_nhbr, z.caret, Zipper.move(Right, z)) {
    | (Some(p), Inner(_), Some(z))
        when Piece.is_grout(p) && new_nhbr != init_left_nhbr => z
    | _ => z
    };
  | (Right, Inner(idx), (_, Some(t))) when is_last_inner_pos(t, idx) =>
    let* z = Insert.replace_shard(Right, Token.rm_nth(idx + 1, t), z);
    z |> Zipper.Caret.set(Outer) |> Zipper.move(Right);
  | (Right, Inner(idx), (_, Some(t))) =>
    Insert.replace_shard(Right, Token.rm_nth(idx + 1, t), z)
  | (Left | Right, Inner(_), (_, None)) =>
    /* Counterintuitve, but Left and Right are identically handled */
    z |> Zipper.Caret.set(Outer) |> Zipper.delete(Right)
  | (Left, Outer, (Some(t), _)) when Token.length(t) > 1 =>
    Insert.replace_shard(Left, Token.rm_last(t), z)
  | (Right, Outer, (_, Some(t))) when Token.length(t) > 1 =>
    Insert.replace_shard(Right, Token.rm_first(t), z)
  | (Left | Right, Outer, (None | Some(_), _)) => z |> Zipper.delete(d) /* t.length == 1 */
  };
};

let merge = ((l, r): (Token.t, Token.t), z: t): option(t) => {
  /* Note: Below order causes it to retain id of right tile */
  let* z = Zipper.delete(Left, z);
  let+ z = Insert.replace_shard(Right, Token.append(l, r), z);
  let z = Zipper.Caret.set(Inner(Token.length(l) - 1), z);
  /* Regrouting direction needed to merge prefixs into infix eg ! */
  remold_regrout(Right, z);
};

let parent_duomerge = (~id: Id.t, lbl: Label.t, z: t): t => {
  z
  |> Zipper.delete_parent
  |> Zipper.Caret.set(Inner(0))
  |> Zipper.construct(~id, ~d=Right, ~backpack=Left, lbl)
  /* Below regrouting important for parens/ap positioning */
  |> remold_regrout(Right);
};

/* Check if containing duo form has a mono equivalent e.g. list literals */
let parent_duomerges = (z: Zipper.t) => {
  let* parent = Relatives.parent(z.relatives);
  let* lbl = Piece.label(parent);
  let+ res = Token.duomerges(lbl);
  (res, Piece.id(parent));
};

let go = (d: Direction.t, z: t): option(t) => {
  let* z = destruct(d, z);
  switch (parent_duomerges(z), Zipper.neighbor_shards(z)) {
  | (Some((lbl, id)), _) when Siblings.no_siblings(z.relatives.siblings) =>
    /* Merge only when containing segment is totally empty after delete */
    Some(parent_duomerge(~id, lbl, z))
  | (_, (Some(l), Some(r)))
      when Token.is_potential_token(Token.append(l, r)) && z.caret == Outer =>
    z |> merge((l, r))
  | _ =>
    z |> Insert.expand_or_barf_neighbors |> remold_regrout(d) |> Option.some
  };
};
