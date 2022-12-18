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
    |> Zipper.directional_destruct(Right)
    |> Option.map(IdGen.id(id_gen));
  let delete_left = z =>
    z |> Zipper.directional_destruct(Left) |> Option.map(IdGen.id(id_gen));
  let construct_right = (lbl, state) =>
    state
    |> Option.map(((z, id_gen)) => Zipper.construct(Right, lbl, z, id_gen));
  let construct_left = (lbl, state) =>
    state
    |> Option.map(((z, id_gen)) => Zipper.construct(Left, lbl, z, id_gen));
  switch (d, caret, neighbor_monotiles((l_sibs, r_sibs))) {
  /* When there's a selection, defer to Outer */
  | _ when z.selection.content != [] =>
    z |> Zipper.destruct |> IdGen.id(id_gen) |> Option.some
  /* Special cases for list literals. When deletion would
     effect an empty list, we convert it to non-empty  */
  | (Left, Outer, (Some(t), _)) when Form.is_empty_list(t) =>
    z |> delete_left |> construct_left(Form.listlit_lbl)
  | (Right, Outer, (_, Some(t))) when Form.is_empty_list(t) =>
    z |> delete_right |> construct_right(Form.listlit_lbl)
  | (Left, Inner(_, 0), (_, Some(t))) when Form.is_empty_list(t) =>
    z |> delete_right |> construct_right(Form.listlit_lbl)
  | (Right, Inner(_, n), (_, Some(t)))
      when Form.is_empty_list(t) && n == last_inner_pos(t) =>
    z |> delete_right |> construct_left(Form.listlit_lbl)
  /* Special cases for string literals. When deletion would
     remove an outer quote, we instead remove the whole string */
  | (Left, Outer, (Some(t), _)) when Form.is_string(t) => delete_left(z)
  | (Right, Outer, (_, Some(t))) when Form.is_string(t) => delete_right(z)
  | (Left, Inner(_, 0), (_, Some(t))) when Form.is_string(t) =>
    delete_right(z)
  | (Right, Inner(_, n), (_, Some(t)))
      when Form.is_string(t) && n == last_inner_pos(t) =>
    delete_right(z)
  /* Remove inner character */
  | (Left, Inner(_, c_idx), (_, Some(t))) =>
    let z = Zipper.update_caret(Zipper.Caret.decrement, z);
    Zipper.replace(Right, [Token.rm_nth(c_idx, t)], (z, id_gen));
  | (Right, Inner(_, c_idx), (_, Some(t))) when c_idx == last_inner_pos(t) =>
    Zipper.replace(Right, [Token.rm_nth(c_idx + 1, t)], (z, id_gen))
    |> OptUtil.and_then(((z, id_gen)) =>
         z
         |> Zipper.set_caret(Outer)
         |> Zipper.move(Right)
         |> Option.map(IdGen.id(id_gen))
       )
  /* If not on last inner position */
  | (Right, Inner(_, c_idx), (_, Some(t))) =>
    Zipper.replace(Right, [Token.rm_nth(c_idx + 1, t)], (z, id_gen))
  /* Can't subdestruct in delimiter, so just destruct on whole delimiter */
  | (Left, Inner(_), (_, None))
  | (Right, Inner(_), (_, None)) =>
    /* Note: Counterintuitve, but yes, these cases are identically handled */
    z
    |> Zipper.set_caret(Outer)
    |> Zipper.directional_destruct(Right)
    |> Option.map(IdGen.id(id_gen))
  //| (_, Inner(_), (_, None)) => None
  | (Left, Outer, (Some(t), _)) when Token.length(t) > 1 =>
    //Option.map(IdGen.id(id_gen)
    Zipper.replace(Left, [Token.rm_last(t)], (z, id_gen))
  | (Right, Outer, (_, Some(t))) when Token.length(t) > 1 =>
    Zipper.replace(Right, [Token.rm_first(t)], (z, id_gen))
  | (_, Outer, (Some(_), _)) /* t.length == 1 */
  | (_, Outer, (None, _)) =>
    z |> Zipper.directional_destruct(d) |> Option.map(IdGen.id(id_gen))
  };
};

let merge =
    ((l, r): (Token.t, Token.t), (z, id_gen): state): option(state) =>
  z
  |> Zipper.set_caret(Inner(0, Token.length(l) - 1))  // note monotile assumption
  |> Zipper.directional_destruct(Left)
  |> OptUtil.and_then(Zipper.directional_destruct(Right))
  |> Option.map(z => Zipper.construct(Right, [l ++ r], z, id_gen));

/* Check if containing form has an empty form e.g. empty list literals */
let parent_is_mergable = (z: Zipper.t) =>
  switch (Relatives.parent(z.relatives)) {
  | Some(Tile({label, _})) when label == Form.listlit_lbl =>
    Some([Form.empty_list])
  | _ => None
  };

let go = (d: Direction.t, (z, id_gen): state): option(state) => {
  let* (z, id_gen) = destruct(d, (z, id_gen));
  let z_trimmed = update_siblings(Siblings.trim_whitespace_and_grout, z);
  switch (
    z.caret,
    neighbor_monotiles(z_trimmed.relatives.siblings),
    parent_is_mergable(z),
  ) {
  | (Outer, (None, None), Some(lbl)) =>
    z
    |> Zipper.delete_parent
    |> Zipper.set_caret(Inner(List.length(lbl), 0))
    |> (z => Zipper.construct(Right, lbl, z, id_gen))
    |> Option.some
  | (Outer, (Some(l), Some(r)), _) when Form.is_valid_token(l ++ r) =>
    merge((l, r), (z_trimmed, id_gen))
  | _ => Some((z, id_gen))
  };
};
