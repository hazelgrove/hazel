open Zipper;
open Util;
open OptUtil.Syntax;

let barf = (d: Direction.t, (z, id_gen): state): option(state) => {
  /* Removes the d-neighboring tile and drops from backpack;
     precondition: the d-neighbor should be a monotile
     string-matching the dropping shard */
  let* z = select(d, z);
  let+ z = z |> Zipper.destruct |> Zipper.put_down(d);
  (z, id_gen);
};

let expand =
    (kw: Token.t, d: Direction.t, (z, id_gen): state): option(state) => {
  /* Removes the d-neighboring tile and reconstructs it, triggering
     keyword-expansion; precondition: the d-neighbor should be a monotile
     string-matching a keyword of an expanding form */
  let (new_label, new_dir) = Molds.delayed_expansion(kw, d);
  let* z = select(d, z);
  switch (d) {
  | Left => Some(construct(new_dir, new_label, z, id_gen))
  | Right =>
    let (z, id_gen) = construct(new_dir, new_label, z, id_gen);
    let* z = Zipper.move(Left, z);
    /* NOTE(andrew): This movement could likely be internalized into
       construct; however the notion of direction in these functions is
       currently overloaded, possibly with compensating errors, and
       I'm not currently confident making this change. */
    Some((z, id_gen));
  };
};

let expand_or_barf_left_neighbor = ((z, _) as s: state): option(state) =>
  /* If left neighbor is a monotile (a) string-matching the shard at the
     top of the backpack, barf it, or (b) an expansing keyword, expand it. */
  switch (left_neighbor_monotile(z.relatives.siblings)) {
  | Some(kw) when Backpack.will_barf(kw, z.backpack) => barf(Left, s)
  | Some(kw) when Molds.will_expand(kw) => expand(kw, Left, s)
  | _ => Some(s)
  };

let expand_or_barf_right_neighbor = ((z, _) as s: state): option(state) =>
  /* If right neighbor is a monotile (a) string-matching the shard at the
     top of the backpack, barf it, or (b) an expansing keyword, expand it. */
  switch (right_neighbor_monotile(z.relatives.siblings)) {
  | Some(kw) when Backpack.will_barf(kw, z.backpack) => barf(Right, s)
  | Some(kw) when Molds.will_expand(kw) => expand(kw, Right, s)
  | _ => Some(s)
  };

let make_new_tile =
    (t: Token.t, direction_pref: Direction.t, z: t): IdGen.t(t) =>
  /* Adds a new tile at the caret. If the new token matches the top
     of the backpack, the backpack shard is dropped. Otherwise, we
     construct a new tile, which may immediately expand. */
  switch (Backpack.will_barf(t, z.backpack), Zipper.put_down(Left, z)) {
  | (true, Some(z')) => IdGen.return(z')
  | _ =>
    let (lbl, direction) = Molds.instant_expansion(t, direction_pref);
    Zipper.construct(direction, lbl, z);
  };

let expand_neighbors_and_make_new_tile =
    (char: Token.t, state: state): option(state) => {
  /* Trigger a token boundary event and create a new tile.
     This process proceeds left-to-right, potentially involving both
     neighboring tiles, and potentially triggering up to 3 expansion
     or backpack barf events. In particular, both left and right
     neighboring monotiles may undergo delayed (aka keyword) expansion,
     and the newly-created single-character token may undergo instant
     expansion. Note that this left-to-right order is load-bearing;
     consider for example what will happen given "(if|then" if the ")"
     in then backpack is dropped at the caret. */
  let* (z, id_gen) = expand_or_barf_left_neighbor(state);
  make_new_tile(char, Left, z, id_gen) |> expand_or_barf_right_neighbor;
};

let replace_tile =
    (t: Token.t, d: Direction.t, (z, id_gen): state): option(state) => {
  let+ z = Zipper.directional_destruct(d, z);
  make_new_tile(t, d, z, id_gen);
};

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

let insert_outer = (char: string, (z, _) as state: state): option(state) =>
  switch (sibling_appendability(char, z.relatives.siblings)) {
  | AppendNeither => expand_neighbors_and_make_new_tile(char, state)
  | AppendLeft(t) => replace_tile(t, Left, state)
  | AppendRight(t) => replace_tile(t, Right, state)
  };

let split =
    ((z, id_gen): state, char: string, idx: int, t: Token.t): option(state) => {
  let (l, r) = Token.split_nth(idx, t);
  z
  |> Zipper.set_caret(Outer)
  |> Zipper.select(Right)
  |> Option.map(z => Zipper.construct(Right, [r], z, id_gen))  //overwrite right
  |> Option.map(((z, id_gen)) => Zipper.construct(Left, [l], z, id_gen))
  |> OptUtil.and_then(expand_neighbors_and_make_new_tile(char));
};

let opt_regrold = d =>
  Option.map(((z, id_gen)) => remold_regrout(d, z, id_gen));

let move_into_if_stringlit = (char, z) =>
  /* This is special-case logic for advancing the caret to position between the quotes
     in newly-created stringlits. The main stringlit special-case is in Zipper.constuct
     and ideally this logic would be located there as well, but both regrouting and
     subsequent caret position logic at this function's callsites dicate that this
     be done after. Not too happy about this tbh. */
  Form.is_string_delim(char)
    ? switch (move(Left, z)) {
      | None => z
      | Some(z) => z |> set_caret(Inner(0, 0))
      }
    : z;

let go =
    (
      char: string,
      ({caret, relatives: {siblings, _}, _} as z, id_gen): state,
    )
    : option(state) => {
  /* If there's a selection, delete it before proceeding */
  let z = z.selection.content != [] ? Zipper.destruct(z) : z;
  switch (caret, neighbor_monotiles(siblings)) {
  /* Special cases for insertion of quotes when the caret is
     in or is adjacent to a string. This is necessary to
     avoid breaking paste. */
  | (_, (_, Some(t))) when Form.is_string(t) && Form.is_string_delim(char) =>
    z
    |> Zipper.set_caret(Outer)
    |> Zipper.move(Right)
    |> Option.map(z => (z, id_gen))
  | (Outer, (Some(t), _))
      when Form.is_string(t) && Form.is_string_delim(char) =>
    Some((z, id_gen))
  | (Inner(d_idx, n), (_, Some(t))) =>
    let idx = n + 1;
    let new_t = Token.insert_nth(idx, char, t);
    /* If inserting wouldn't produce a valid token, split */
    Form.is_valid_token(new_t)
      ? z
        |> Zipper.set_caret(Inner(d_idx, idx))
        |> (z => Zipper.replace(Right, [new_t], (z, id_gen)))
        |> opt_regrold(Left)
      : split((z, id_gen), char, idx, t) |> opt_regrold(Right);
  /* Can't insert inside delimiter */
  | (Inner(_, _), (_, None)) => None
  | (Outer, (_, Some(_))) =>
    let caret: Zipper.Caret.t =
      /* If we're adding to the right, move caret inside right nhbr */
      switch (sibling_appendability(char, siblings)) {
      | AppendRight(_) => Inner(0, 0) //Note: assumption of monotile
      | AppendNeither
      | AppendLeft(_) => Outer
      };
    (z, id_gen)
    |> insert_outer(char)
    |> Option.map(((z, id_gen)) => (Zipper.set_caret(caret, z), id_gen))
    |> opt_regrold(Left)
    |> Option.map(((z, id_gen)) =>
         (move_into_if_stringlit(char, z), id_gen)
       );
  | (Outer, (_, None)) =>
    insert_outer(char, (z, id_gen))
    |> opt_regrold(Left)
    |> Option.map(((z, id_gen)) =>
         (move_into_if_stringlit(char, z), id_gen)
       )
  };
};
