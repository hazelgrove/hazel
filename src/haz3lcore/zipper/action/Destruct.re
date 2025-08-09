open Zipper;
open Util;
open OptUtil.Syntax;

let destruct = (d: Direction.t, z: t): option(t) => {
  let last_inner_pos = t => Token.length(t) - 2;
  let delete_right = z =>
    z |> Zipper.set_caret(Outer) |> Zipper.delete(Right);
  let construct_right = (l, s) =>
    Option.map(Zipper.construct(~caret=Right, ~backpack=Right, l), s);
  let construct_left = (l, s) =>
    Option.map(Zipper.construct(~caret=Left, ~backpack=Left, l), s);
  switch (d, z.caret, Zipper.neighbor_shards(z)) {
  /* When there's a selection, defer to Outer */
  | _ when z.selection.content != [] => z |> Zipper.destruct |> Option.some
  /* Special cases for mono forms which can split into duo forms,
     e.g. list literals. When deletion would alter the mono form,
     we replace it to the corresponding duo form.  */
  | (Left, Outer, (Some(t), _)) when Token.duosplits(t) != [] =>
    z |> Zipper.delete(Left) |> construct_left(Token.duosplits(t))
  | (Right, Outer, (_, Some(t))) when Token.duosplits(t) != [] =>
    z |> delete_right |> construct_right(Token.duosplits(t))
  | (Left, Inner(_, 0), (_, Some(t))) when Token.duosplits(t) != [] =>
    z |> delete_right |> construct_right(Token.duosplits(t))
  | (Right, Inner(_, n), (_, Some(t)))
      when Token.duosplits(t) != [] && n == last_inner_pos(t) =>
    z |> delete_right |> construct_left(Token.duosplits(t))
  /* Special cases for string literals. When deletion would
     remove an outer quote, we instead remove the whole string */
  | (Left, Outer, (Some(t), _))
      when Token.is_string(t) || Token.is_comment(t) =>
    Zipper.delete(Left, z)
  | (Right, Outer, (_, Some(t)))
      when Token.is_string(t) || Token.is_comment(t) =>
    delete_right(z)
  | (Left, Inner(_, 0), (_, Some(t))) when Token.is_string(t) =>
    delete_right(z)
  | (Left, Inner(_, 0), (_, Some(t)))
      when Token.is_string(t) || Token.is_comment(t) =>
    delete_right(z)
  | (Right, Inner(_, n), (_, Some(t)))
      when
        (Token.is_string(t) || Token.is_comment(t))
        && n == last_inner_pos(t) =>
    delete_right(z) /* Remove inner character */
  | (Left, Inner(delim, c_idx), (_, Some(t))) =>
    let z =
      Zipper.update_caret(
        fun
        | Outer
        | Inner(_, 0) => Outer
        | Inner(_d, c) => Inner(0, c - 1),
        z,
      );
    let+ z = Insert.replace_shard(Right, Token.rm_nth(c_idx, t), z);
    if (delim == 0) {
      z;
    } else {
      /* Edge case */
      let z = Insert.expand_or_barf_neighbors(z);
      let init_left_nhbr = Siblings.right_neighbor(z.relatives.siblings);
      let z = remold_regrout(d, z);
      let new_nhbr = Siblings.right_neighbor(z.relatives.siblings);
      switch (new_nhbr, z.caret) {
      | (Some(p), Inner(_))
          when Piece.is_grout(p) && new_nhbr != init_left_nhbr =>
        switch (Zipper.move(Right, z)) {
        | None => z
        | Some(z) => z
        }
      | _ => z
      };
    };
  | (Right, Inner(_, c_idx), (_, Some(t))) when c_idx == last_inner_pos(t) =>
    Insert.replace_shard(Right, Token.rm_nth(c_idx + 1, t), z)
    |> OptUtil.and_then(z =>
         z |> Zipper.set_caret(Outer) |> Zipper.move(Right)
       ) /* If not on last inner position */
  | (Right, Inner(_, c_idx), (_, Some(t))) =>
    Insert.replace_shard(Right, Token.rm_nth(c_idx + 1, t), z)
  /* Can't subdestruct in delimiter, so just destruct on whole delimiter */
  | (Left, Inner(_), (_, None))
  | (Right, Inner(_), (_, None)) =>
    /* Note: Counterintuitve, but yes, these cases are identically handled */
    z |> Zipper.set_caret(Outer) |> Zipper.delete(Right)
  | (Left, Outer, (Some(t), _)) when Token.length(t) > 1 =>
    Insert.replace_shard(Left, Token.rm_last(t), z)
  | (Right, Outer, (_, Some(t))) when Token.length(t) > 1 =>
    Insert.replace_shard(Right, Token.rm_first(t), z)
  | (_, Outer, (Some(_), _)) /* t.length == 1 */
  | (_, Outer, (None, _)) => z |> Zipper.delete(d)
  };
};

let merge = ((l, r): (Token.t, Token.t), z: t): option(t) => {
  /* Note: Below order causes it to retain id of right tile */
  let* z = Zipper.delete(Left, z);
  let+ z = Insert.replace_shard(Right, Token.append(l, r), z);
  let z =
    Zipper.set_caret(Inner(Zipper.delim_idx(z), Token.length(l) - 1), z);
  /* Regrouting direction needed to merge prefixs into infix eg ! */
  remold_regrout(Right, z);
};

let parent_duomerge = (~id: Id.t, lbl: Label.t, z: t): t => {
  z
  |> Zipper.delete_parent
  |> Zipper.set_caret(Inner(0, 0))  /* Note duotile assumption */
  |> Zipper.construct(~id, ~caret=Right, ~backpack=Left, lbl)
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
