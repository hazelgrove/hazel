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

let barf = (d: Direction.t, (z, id_gen): state): option(state) =>
  /* Removes the d-neighboring tile and drops from backpack;
     precondition: the d-neighbor should be a monotile
     string-matching to the dropping shard */
  select(d, z)
  |> Option.map(Zipper.destruct)
  |> OptUtil.and_then(Zipper.put_down)
  |> Option.map(z => (z, id_gen));

let expand =
    (kw: Token.t, d: Direction.t, (z, id_gen): state): option(state) => {
  /* Removes the d-neighboring tile and reconstructs it, triggering
     keyword-expansion; precondition: the d-neighbor should be a monotile
     string-matching a keyword of an expanding form */
  let (new_label, new_dir) = Molds.delayed_expansion(kw, d);
  select(d, z) |> Option.map(z => construct(new_dir, new_label, z, id_gen));
};

let expand_or_barf_neighbors = ((z, _) as s: state): option(state) =>
  /* If either neighbor is a monotile (a) string-matching the shard at the top
     of the backpack, barf it, or (b) an expansing keyword, expand it. Only one
     neighbor is affected; this can probably be relaxed. TODO(andrew) */
  switch (neighbor_monotiles(z.relatives.siblings)) {
  | (Some(kw), _) when Backpack.will_barf(kw, z.backpack) => barf(Left, s)
  | (_, Some(kw)) when Backpack.will_barf(kw, z.backpack) => barf(Right, s)
  | (Some(kw), _) when Molds.will_expand(kw) => expand(kw, Left, s)
  | (_, Some(kw)) when Molds.will_expand(kw) => expand(kw, Right, s)
  | _ => Some(s)
  };

let expand_or_barf_neighbors2 = ((z, _) as s: state): option(state) =>
  /* If either neighbor is a monotile (a) string-matching the shard at the top
     of the backpack, barf it, or (b) an expansing keyword, expand it. Only one
     neighbor is affected; this can probably be relaxed. TODO(andrew) */
  switch (neighbor_monotiles(z.relatives.siblings)) {
  | (Some(kw), _) when Backpack.will_barf(kw, z.backpack) => barf(Left, s)
  | (_, Some(kw)) when Backpack.will_barf(kw, z.backpack) => barf(Right, s)
  | (Some(kw), _) when Molds.will_expand(kw) => expand(kw, Left, s)
  | (_, Some(kw)) when Molds.will_expand(kw) => expand(kw, Right, s)
  | _ => Some(s)
  };

let barf_or_construct =
    (t: Token.t, direction_pref: Direction.t, z: t): IdGen.t(t) => {
  let barfed = Backpack.will_barf(t, z.backpack) ? Zipper.put_down(z) : None;
  switch (barfed) {
  | Some(z) => IdGen.return(z)
  | None =>
    let (lbl, direction) = Molds.instant_expansion(t, direction_pref);
    Zipper.construct(direction, lbl, z);
  };
};

let expand_and_barf_or_construct = (char: string, state: state) =>
  state
  |> expand_or_barf_neighbors
  |> Option.map(((z, id_gen)) => barf_or_construct(char, Left, z, id_gen));

let insert_outer = (char: string, (z, id_gen): state): option(state) =>
  switch (sibling_appendability(char, z.relatives.siblings)) {
  | AppendNeither => expand_and_barf_or_construct(char, (z, id_gen))
  | AppendLeft(new_t) =>
    z
    |> Zipper.directional_destruct(Left)
    |> Option.map(z => barf_or_construct(new_t, Left, z, id_gen))
  | AppendRight(new_t) =>
    z
    |> Zipper.directional_destruct(Right)
    |> Option.map(z => barf_or_construct(new_t, Right, z, id_gen))
  };

let split =
    ((z, id_gen): state, char: string, idx: int, t: Token.t): option(state) => {
  let (l, r) = Token.split_nth(idx, t);
  z
  |> Zipper.set_caret(Outer)
  |> Zipper.select(Right)
  |> Option.map(z => Zipper.construct(Right, [r], z, id_gen))  //overwrite right
  |> Option.map(((z, id_gen)) => Zipper.construct(Left, [l], z, id_gen))
  |> OptUtil.and_then(expand_and_barf_or_construct(char));
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
