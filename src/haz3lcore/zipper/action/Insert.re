open Zipper;
open Util;
open OptUtil.Syntax;

/* Get the form label a token expands into, and the direction
 * that expansion should happen in. This is rightwards for leading
 * expanding delimiters, leftwards for trailing delimiters. This
 * is mostly a wrapper around Form.Expansion; the additional logic
 * hers handles one special case of sort-dependendent expansion  */
let expansion = (t: Token.t, z: t): (Label.t, Direction.t) => {
  let before_case_shard = (z: t): bool =>
    List.exists(
      (p: Piece.t) =>
        switch (p) {
        | Tile({label: ["case", "end"], shards: [0], _}) => true
        | _ => false
        },
      z.relatives.siblings |> fst,
    );
  let inside_case = (z: t): bool =>
    switch (Ancestors.parent(z.relatives.ancestors)) {
    | Some({label: ["case", "end"], _}) => true
    | _ => false
    };
  switch (t) {
  | _ when Token.is_string_delim(t) || Token.is_quoted_label_delim(t) =>
    /* Special case for constructing string/label literals. */
    ([t ++ t], Left)
  | "|" when !(before_case_shard(z) || inside_case(z)) =>
    /* Only expand case rules when inside a case */
    ([t], Left)
  | _ => Form.Expansion.get(t)
  };
};

/* Insert a new shard based on token `t` on the `d`-side of the caret */
let insert_shard = (~id: Id.t, ~d: Direction.t, t: Token.t, z: t, ~root): t => {
  let z = destroy_selection(z);
  if (Token.is_secondary(t)) {
    Zipper.put_down_seg(d, [Piece.mk_secondary(id, t)], z);
  } else if (Zipper.backpack_find(t, z) != None) {
    let target = Zipper.backpack_find(t, z) |> Option.get;
    Zipper.put_down_target(d, target, z, ~root);
  } else {
    let (label, delim_d) = expansion(t, z);
    let molds = Form.Molds.get(label);
    assert(molds != []);
    let mold = List.hd(molds);
    let shard =
      Tile.split_shards(id, label, mold, List.mapi((i, _) => i, label))
      |> (delim_d == Right ? ListUtil.last : List.hd);
    Zipper.put_down_seg(d, [Tile(shard)], z);
  };
};

/* Replace `d`-neighbor shard with a new one based on token `t` */
let replace_shard = (d: Direction.t, t: Token.t, z: t, ~root): option(t) => {
  let id = Zipper.adjacent_monotile_or_new_id(d, z);
  let+ z = delete(d, z);
  insert_shard(~id, ~d, t, z, ~root);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type appendability = option((Direction.t, Token.t));

/* This papers over an edge case which needs non-local rematching
 * to handle properly. Consider inserting an opening parens at `f(g|)`.
 * Without this check, this would result in the closing parens being
 * rematched with the inserted opening parens, orphaning the leftmost
 * opening parens. This in itself is fine. However, when entering
 * the subsequent closing parens in the middle of the resulting empty
 * ap, it will not be rematched, as the closing parens will be inside
 * the bidelmited segment of the ap, and so will not rematch with the
 * opening parens outside it. In principle this could be resolved in
 * two ways. Either with a more complete reparsing solution which
 * matches across segments, or by non-local rematching within a
 * segment (if we set it up so the inserted closing parens matches
 * with the adjacent opening one and thus does not end up an orphan
 * in the bidelimited segement). In absence of either of these
 * mechanisms, we need this hack is required. */
let parens_edge_case = (char: string, z: t): bool =>
  switch (
    char,
    Zipper.neighbor_token(Right, z),
    Siblings.neighbor(Right, z.relatives.siblings),
  ) {
  | ("[", Some("]"), None)
  | ("(", Some(")"), None) => true
  | _ => false
  };

/* Decide which if any sibling we can append `char` to.
 * We bias towards the left sibling */
let sibling_appendability = (char: string, z: t): appendability =>
  switch (neighbor_tokens(z)) {
  | (Some(t), _)
      when
        Token.is_potential_token(Token.append(t, char))
        && !parens_edge_case(char, z) =>
    Some((Left, Token.append(t, char)))
  | (_, Some(t))
      when
        Token.is_potential_token(Token.append(char, t))
        && !parens_edge_case(char, z) =>
    Some((Right, Token.append(char, t)))
  | _ => None
  };

/* If the insertion will 'fill a hole', i.e. replace an
 * existing grout,we make a best-effort approach to transfer
 * the UUID. See also Destruct.capture */
let preserve_grout_id = (char: string, z: t): (Id.t, t) =>
  switch (Siblings.neighbors(z.relatives.siblings)) {
  | _ when Token.is_comment_delim(char) || Token.is_secondary(char) => (
      Id.mk(),
      z,
    )
  | (Some(Grout(g)), _) => (
      g.id,
      update_siblings(((l, r)) => (l |> ListUtil.split_last |> fst, r), z),
    )
  | (_, Some(Grout(g))) => (
      g.id,
      update_siblings(((l, r)) => (l, List.tl(r)), z),
    )
  | _ => (Id.mk(), z)
  };

/* Figure out if we should avoid inserting a space
 * because grout is due to be inserted instead,
 *  e.g. when splitting `[|]` or `(|)` */
let should_supress_space = (z: t, ~root): bool =>
  switch (
    Siblings.neighbor(
      Left,
      remold_regrout(Right, z, ~root).relatives.siblings,
    )
  ) {
  | None => false
  | Some(p) => Piece.is_grout(p)
  };

/* This is special-case logic for advancing the caret to between
 * the quotes in newly-created stringlits. This should be done
 * before regrouting to avoid annoying edge cases. */
let move_into_string_or_comment = (char: string, z: t): t =>
  Token.is_string_or_comment_delim(char)
    ? switch (move(Left, z)) {
      | None => z
      | Some(z) => z |> Caret.set(Inner(0))
      }
    : z;

/* Split creates three tokens; two from splitting the existing one,
 * and a new single-character token (or grout) in the middle. */
let split = (z: t, char: string, idx: int, t: Token.t, ~root): option(t) => {
  let insert_shard = insert_shard(~root);
  let (l, r) = Token.split_nth(idx, t);
  let id = Zipper.adjacent_monotile_or_new_id(Right, z);
  let+ z = z |> Caret.set(Outer) |> Zipper.delete(Right);
  let z =
    /* If both are leading expanders, we want to prevent
     * possible theft of trailing delimiters; see Issue #1907.
     * Otherwise however we want to process these ltr as the
     * rightwards may be a trailing delim of the leftwards. */
    Form.Expansion.is_leading(l) && Form.Expansion.is_leading(r)
      ? z
        |> insert_shard(~id=Id.mk(), ~d=Right, r)
        |> insert_shard(~id, ~d=Left, l)
      : z
        |> insert_shard(~id, ~d=Left, l)
        |> insert_shard(~id=Id.mk(), ~d=Right, r);
  let z =
    Token.space == char && should_supress_space(z, ~root)
      ? z
      : z
        |> insert_shard(~id=Id.mk(), ~d=Left, char)
        |> move_into_string_or_comment(char);
  remold_regrout(Right, z, ~root);
};

/* If the caret is precisely between two tokens, which
 * can become a valid token if merged, merge those tokens */
let will_merge = (z: t): option((Token.t, Token.t)) =>
  switch (Zipper.neighbor_tokens(z)) {
  | (Some(l), Some(r))
      when Token.is_potential_token(Token.append(l, r)) && z.caret == Outer =>
    Some((l, r))
  | _ => None
  };

/* If the caret is precisely between two tokens, which
 * can become a valid token if merged, merge those tokens */
let merge_or_noop = (z: t, ~root): t =>
  switch (will_merge(z)) {
  | Some((l, r)) =>
    /* We remove the left manually, and then replace the right */
    let z = Zipper.delete(Left, z) |> Option.get;
    let z = replace_shard(Right, Token.append(l, r), z, ~root) |> Option.get;
    let z = Caret.set(Inner(Token.length(l) - 1), z);
    /* Regrouting direction needed to merge prefixs into infix eg ! */
    remold_regrout(Right, z, ~root);
  | None => z
  };

/* If a grout is due to be inserted to the right of the caret,
 * when the caret position will end up inside a token, we want
 * to keep the caret inside the current token, not put it on the
 * new grout. I hope for a clearer way to handle this case but I
 * haven't found it; it may just be a necessary consequence of
 * the way inner caret index is decoupled from the zipper cursor. */
let adjust_caret_pos = (~z_final: t, ~z_init: t): t => {
  let init_nhbr = Siblings.neighbor(Right, z_init.relatives.siblings);
  let final_nhbr = Siblings.neighbor(Right, z_final.relatives.siblings);
  switch (final_nhbr, z_final.caret, Zipper.move(Right, z_final)) {
  | (Some(p), Inner(_), Some(z_moved))
      when Piece.is_grout(p) && final_nhbr != init_nhbr => z_moved
  | _ => z_final
  };
};

/* If char can be appended to either sibling token, do it,
 * otherwise insert a new `char` token */
let insert_or_append = (char: string, z: t, ~root): option(t) => {
  let+ z_init =
    switch (sibling_appendability(char, z)) {
    | None =>
      let (id, z) = preserve_grout_id(char, z);
      Some(insert_shard(~id, ~d=Left, char, z, ~root));
    | Some((d, t)) => replace_shard(d, t, z, ~root)
    };
  let z_final =
    z_init
    |> move_into_string_or_comment(char)
    |> remold_regrout(Left, ~root)
    |> merge_or_noop(~root);
  adjust_caret_pos(~z_final, ~z_init);
};

let go = (char: string, z: t, ~root): option(t) => {
  /* If there's a selection, delete it before proceeding */
  let z = z.selection.content != [] ? Zipper.destroy_selection(z) : z;
  switch (z.caret, neighbor_tokens(z)) {
  /* If we try to insert a quote inside an existing string, or a #
   * in a comment, we are instead moved to the righthand side of
   * the operand. Note that this behavior is load-bearing for the
   * current parsing approach including Paste */
  | (_, (_, Some(t))) when Token.closing_stringlit_or_comment(char, t) =>
    z |> Caret.set(Outer) |> Zipper.move(Right)
  | (Outer, (Some(t), _)) when Token.closing_stringlit_or_comment(char, t) =>
    Some(z)
  | (Inner(idx), (_, Some(t))) =>
    let idx = idx + 1;
    let new_token = Token.insert_nth(idx, char, t);
    let z = Caret.set(Inner(idx), z);
    Token.is_potential_token(new_token)
      ? z
        |> replace_shard(Right, new_token, ~root)
        |> Option.map(remold_regrout(Right, ~root))
      : split(z, char, idx, t, ~root);
  | (Inner(_), (_, None)) => None
  | (Outer, _) =>
    let z =
      Caret.set(
        switch (sibling_appendability(char, z)) {
        | Some((Right, _)) => Inner(0)
        | None
        | Some((Left, _)) => Outer
        },
        z,
      );
    insert_or_append(char, z, ~root);
  };
};

/* This is a wrapper intended to effectuate after-insertion conditional
 * operations. See Triggers.re for more details */
let go =
    (~ci: option(Language.Info.t)=None, char: string, z: t, ~root)
    : option(t) => {
  let+ z = go(char, z, ~root);
  Triggers.insert(~ci, z);
};
