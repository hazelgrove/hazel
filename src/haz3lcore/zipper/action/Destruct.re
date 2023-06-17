open Zipper;
open Util;
open OptUtil.Syntax;

let destruct =
    (
      d: Direction.t,
      ({caret, relatives: {siblings: (l_sibs, r_sibs), _}, _} as z, id_gen): state,
    )
    : option(state) => {
  /* Could add checks on valid tokens (all of these hold assuming substring) */
  let last_inner_pos = t => Token.length(t) - 2;
  let delete_right = z =>
    z
    |> Zipper.set_caret(Outer)
    |> Zipper.delete(Right)
    |> Option.map(IdGen.id(id_gen));
  let delete_left = z =>
    z |> Zipper.delete(Left) |> Option.map(IdGen.id(id_gen));
  let construct_right = (l, s) =>
    Option.map(
      ((z, id_gen)) =>
        Zipper.construct(~caret=Right, ~backpack=Right, l, z, id_gen),
      s,
    );
  let construct_left = (l, s) =>
    Option.map(
      ((z, id_gen)) =>
        Zipper.construct(~caret=Left, ~backpack=Left, l, z, id_gen),
      s,
    );
  switch (d, caret, neighbor_monotiles((l_sibs, r_sibs))) {
  /* When there's a selection, defer to Outer */
  | _ when z.selection.content != [] =>
    z |> Zipper.destruct |> IdGen.id(id_gen) |> Option.some
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
    Zipper.replace_mono(Right, Token.rm_nth(c_idx, t), (z, id_gen));
  | (Right, Inner(_, c_idx), (_, Some(t))) when c_idx == last_inner_pos(t) =>
    Zipper.replace_mono(Right, Token.rm_nth(c_idx + 1, t), (z, id_gen))
    |> OptUtil.and_then(((z, id_gen)) =>
         z
         |> Zipper.set_caret(Outer)
         |> Zipper.move(Right)
         |> Option.map(IdGen.id(id_gen))
       ) /* If not on last inner position */
  | (Right, Inner(_, c_idx), (_, Some(t))) =>
    Zipper.replace_mono(Right, Token.rm_nth(c_idx + 1, t), (z, id_gen))
  /* Can't subdestruct in delimiter, so just destruct on whole delimiter */
  | (Left, Inner(_), (_, None))
  | (Right, Inner(_), (_, None)) =>
    /* Note: Counterintuitve, but yes, these cases are identically handled */
    z
    |> Zipper.set_caret(Outer)
    |> Zipper.delete(Right)
    |> Option.map(IdGen.id(id_gen))
  //| (_, Inner(_), (_, None)) => None
  | (Left, Outer, (Some(t), _)) when Token.length(t) > 1 =>
    //Option.map(IdGen.id(id_gen)
    Zipper.replace_mono(Left, Token.rm_last(t), (z, id_gen))
  | (Right, Outer, (_, Some(t))) when Token.length(t) > 1 =>
    Zipper.replace_mono(Right, Token.rm_first(t), (z, id_gen))
  | (_, Outer, (Some(_), _)) /* t.length == 1 */
  | (_, Outer, (None, _)) =>
    z |> Zipper.delete(d) |> Option.map(IdGen.id(id_gen))
  };
};

let merge =
    ((l, r): (Token.t, Token.t), (z, id_gen): state): option(state) => {
  z
  |> Zipper.set_caret(Inner(0, Token.length(l) - 1))  // note monotile assumption
  |> Zipper.delete(Left)
  |> OptUtil.and_then(Zipper.delete(Right))
  |> Option.map(z => Zipper.construct_mono(Right, l ++ r, z, id_gen));
};

/* Check if containing duo form has a mono equivalent e.g. list literals */
let parent_duomerges = (z: Zipper.t) => {
  let* parent = Relatives.parent(z.relatives);
  let* lbl = Piece.label(parent);
  Form.duomerges(lbl);
};

let go = (d: Direction.t, (z, id_gen): state): option(state) => {
  let* (z, id_gen) = destruct(d, (z, id_gen));
  let z_trimmed = update_siblings(Siblings.trim_secondary_and_grout, z);
  switch (
    parent_duomerges(z),
    z.caret,
    neighbor_monotiles(z_trimmed.relatives.siblings),
  ) {
  | (Some(lbl), Outer, (None, None))
      when Siblings.no_siblings(z_trimmed.relatives.siblings) =>
    /* Note: we must do the no_siblings check, it does not suffice
       to check no monotile neighbors as there could be other neighbors
       for example edge case: "((|))" */
    z
    |> Zipper.delete_parent
    |> Zipper.set_caret(Inner(List.length(lbl), 0))
    |> (z => Zipper.construct(~caret=Right, ~backpack=Left, lbl, z, id_gen))
    |> Option.some
  | (_, Outer, (Some(l), Some(r)))
      when Form.is_valid_token(l ++ r) && l != "as" =>
    merge((l, r), (z_trimmed, id_gen))
  | _ => Some((z, id_gen))
  };
};
