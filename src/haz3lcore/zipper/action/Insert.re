open Zipper;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type appendability =
  | AppendLeft(Token.t)
  | AppendRight(Token.t)
  | AppendNeither;

let sibling_appendability: (string, Siblings.t) => appendability =
  (char, siblings) =>
    switch (neighbor_monotiles(siblings)) {
    | (Some(t), _) when Form.is_valid_token(t ++ char) =>
      AppendLeft(t ++ char)
    | (_, Some(t)) when Form.is_valid_token(char ++ t) =>
      AppendRight(char ++ t)
    | _ => AppendNeither
    };

let expand_keyword = ((z, _) as state: state): option(state) =>
  /* NOTE(andrew): We may want to allow editing of shards when only 1 of set
     is down (removing the rest of the set from backpack on edit) as something
     like this is necessary for backspace to act as undo after kw-expansion */
  switch (neighbor_monotiles(z.relatives.siblings)) {
  | (Some(kw), _) =>
    let (new_label, direction) = Molds.delayed_completion(kw, Left);
    Outer.replace(direction, new_label, state);
  | _ => Some(state)
  };

let barf_or_construct =
    (t: Token.t, direction_pref: Direction.t, z: t): IdGen.t(t) => {
  let barfed =
    Backpack.is_first_matching(t, z.backpack) ? Outer.put_down(z) : None;
  switch (barfed) {
  | Some(z) => IdGen.return(z)
  | None =>
    let (lbl, direction) = Molds.instant_completion(t, direction_pref);
    Outer.construct(direction, lbl, z);
  };
};

let expand_and_barf_or_construct = (char: string, state: state) =>
  state
  |> expand_keyword
  |> Option.map(((z, id_gen)) => barf_or_construct(char, Left, z, id_gen));

let insert_outer = (char: string, (z, id_gen): state): option(state) =>
  switch (sibling_appendability(char, z.relatives.siblings)) {
  | AppendNeither => expand_and_barf_or_construct(char, (z, id_gen))
  | AppendLeft(new_t) =>
    z
    |> Outer.directional_destruct(Left)
    |> Option.map(z => barf_or_construct(new_t, Left, z, id_gen))
  | AppendRight(new_t) =>
    z
    |> Outer.directional_destruct(Right)
    |> Option.map(z => barf_or_construct(new_t, Right, z, id_gen))
  };

let split =
    ((z, id_gen): state, char: string, idx: int, t: Token.t): option(state) => {
  let (l, r) = Token.split_nth(idx, t);
  z
  |> Caret.set(Outer)
  |> Outer.select(Right)
  |> Option.map(z => Outer.construct(Right, [r], z, id_gen))  //overwrite right
  |> Option.map(((z, id_gen)) => Outer.construct(Left, [l], z, id_gen))
  |> OptUtil.and_then(expand_and_barf_or_construct(char));
};

let opt_regrold = d =>
  Option.map(((z, id_gen)) => remold_regrout(d, z, id_gen));

let go =
    (
      char: string,
      ({caret, relatives: {siblings, _}, _} as z, id_gen): state,
    )
    : option(state) => {
  /* If there's a selection, delete it before proceeding */
  let z = z.selection.content != [] ? Outer.destruct(z) : z;
  switch (caret, neighbor_monotiles(siblings)) {
  | (Inner(d_idx, n), (_, Some(t))) =>
    let idx = n + 1;
    let new_t = Token.insert_nth(idx, char, t);
    /* If inserting wouldn't produce a valid token, split */
    Form.is_valid_token(new_t)
      ? z
        |> Caret.set(Inner(d_idx, idx))
        |> (z => Outer.replace(Right, [new_t], (z, id_gen)))
        |> opt_regrold(Left)
      : split((z, id_gen), char, idx, t) |> opt_regrold(Right);
  /* Can't insert inside delimiter */
  | (Inner(_, _), (_, None)) => None
  | (Outer, (_, Some(_))) =>
    let caret =
      /* If we're adding to the right, move caret inside right nhbr */
      switch (sibling_appendability(char, siblings)) {
      | AppendRight(_) => Inner(0, 0) //Note: assumption of monotile
      | AppendNeither
      | AppendLeft(_) => Outer
      };
    (z, id_gen)
    |> insert_outer(char)
    |> Option.map(((z, id_gen)) => (Caret.set(caret, z), id_gen))
    |> opt_regrold(Left);
  | (Outer, (_, None)) =>
    insert_outer(char, (z, id_gen)) |> opt_regrold(Left)
  };
};
