open Zipper;
open Util;
open OptUtil.Syntax;

let destruct = (d: Direction.t, z: t): option(t) => {
  /* Could add checks on valid tokens (all of these hold assuming substring) */
  let last_inner_pos = t => Token.length(t) - 2;
  let delete_right = z =>
    z |> Zipper.set_caret(Outer) |> Zipper.delete(Right);
  let delete_left = Zipper.delete(Left);
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
  | (Left, Outer, (Some(t), _)) when Form.duosplits(t) != [] =>
    z |> delete_left |> construct_left(Form.duosplits(t))
  | (Right, Outer, (_, Some(t))) when Form.duosplits(t) != [] =>
    z |> delete_right |> construct_right(Form.duosplits(t))
  | (Left, Inner(_, 0), (_, Some(t))) when Form.duosplits(t) != [] =>
    z |> delete_right |> construct_right(Form.duosplits(t))
  | (Right, Inner(_, n), (_, Some(t)))
      when Form.duosplits(t) != [] && n == last_inner_pos(t) =>
    z |> delete_right |> construct_left(Form.duosplits(t))
  /* Special cases for string literals. When deletion would
     remove an outer quote, we instead remove the whole string */
  | (Left, Outer, (Some(t), _))
      when Form.is_string(t) || Form.is_comment(t) =>
    delete_left(z)
  | (Right, Outer, (_, Some(t)))
      when Form.is_string(t) || Form.is_comment(t) =>
    delete_right(z)
  | (Left, Inner(_, 0), (_, Some(t))) when Form.is_string(t) =>
    delete_right(z)
  | (Left, Inner(_, 0), (_, Some(t)))
      when Form.is_string(t) || Form.is_comment(t) =>
    delete_right(z)
  | (Right, Inner(_, n), (_, Some(t)))
      when
        (Form.is_string(t) || Form.is_comment(t)) && n == last_inner_pos(t) =>
    delete_right(z) /* Remove inner character */
  | (Left, Inner(_, c_idx), (_, Some(t))) =>
    let z = Zipper.update_caret(Zipper.Caret.decrement, z);
    Insert.replace_shard(Right, Token.rm_nth(c_idx, t), z);
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

  //| (_, Inner(_), (_, None)) => None
  | (Left, Outer, (Some(t), _)) when Token.length(t) > 1 =>
    Insert.replace_shard(Left, Token.rm_last(t), z)
  | (Right, Outer, (_, Some(t))) when Token.length(t) > 1 =>
    Insert.replace_shard(Right, Token.rm_first(t), z)
  | (_, Outer, (Some(_), _)) /* t.length == 1 */
  | (_, Outer, (None, _)) => z |> Zipper.delete(d)
  };
};

let merge = ((l, r): (Token.t, Token.t), z: t): option(t) => {
  //TODO(andrew):cleanup
  //TODO(andrew): possibly reinstate left as opposed to right id retension
  // let left_monotile_id =
  //   switch (Zipper.adjacent_monotile_id(Left, z)) {
  //   | Some(id) => id
  //   | None => Id.mk()
  //   };
  let z = Zipper.set_caret(Inner(0, Token.length(l) - 1), z); /* Note monotile assumption */
  let* z = Zipper.delete(Left, z);
  //TODO(andrew):cleanup
  //let* z = Zipper.delete(Right, z);
  //let z = Zipper.construct_mono(~id=left_monotile_id, Right, l ++ r, z);
  let* z = Insert.replace_shard(Right, l ++ r, z);
  /* Regrouting direction needed to merge prefixs into infix eg ! */
  let z = remold_regrout(Right, z);
  Some(z);
};

let parent_merge = (~id: Id.t, lbl: Label.t, z: t): t => {
  z
  |> Zipper.delete_parent
  |> Zipper.set_caret(Inner(0, 0))  /* Note 2-token assumption */
  |> Zipper.construct(~id, ~caret=Right, ~backpack=Left, lbl)
  /* Below regrouting important for parens/ap positioning */
  |> remold_regrout(Right);
};

/* Check if containing duo form has a mono equivalent e.g. list literals */
let parent_duomerges = (z: Zipper.t) => {
  let* parent = Relatives.parent(z.relatives);
  let* lbl = Piece.label(parent);
  let+ res = Form.duomerges(lbl);
  (res, Piece.id(parent));
};

let go = (d: Direction.t, z: t): option(t) => {
  let* z = destruct(d, z);
  switch (parent_duomerges(z), Zipper.neighbor_shards(z)) {
  | (Some((lbl, id)), _) when Siblings.no_siblings(z.relatives.siblings) =>
    /* Merge only when containing segment is totally empty after delete */
    Some(parent_merge(~id, lbl, z))
  | (_, (Some(l), Some(r))) when Molds.allow_merge(l, r) && z.caret == Outer =>
    z |> merge((l, r))
  | _ =>
    z |> Insert.expand_or_barf_neighbors |> remold_regrout(d) |> Option.some
  };
};
