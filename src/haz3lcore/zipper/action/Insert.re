open Zipper;
open Util;
open OptUtil.Syntax;

/* Get the form label a token expands into, and the direction
 * that expansion should happen in. This is rightwards for leading
 * expanding delimiters, leftwards for trailing delimiters. This
 * is mostly a wrapper around Form.Expansion; the additional logic
 * here handles special cases of context-dependent expansion. */
let expansion = (sort: Sort.t, t: Token.t, z: t): (Label.t, Direction.t) => {
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
  | "|" when before_case_shard(z) || inside_case(z) =>
    /* SPECIAL CASE: Case rule delimiter.
       Inside a case, always expand | to Rule form regardless of local sort.

       Why this is needed: The Rule form's left nib is Exp (it expects an
       expression). But rule bodies can have type ascriptions like `expr : Type`,
       which means Relatives.sort returns Typ even though semantically we have
       an expression. Sort-specific expansion would fail to find | for Typ.

       This bypasses Form.Expansion.get entirely for | inside case expressions,
       hardcoding the Rule form label. A more principled fix might register |
       for multiple sorts (Exp, Typ, etc.) in Form.Expansion. */
    (["|", "=>"], Left)
  | "|" =>
    /* Outside case: | has no meaning, don't expand */
    ([t], Left)
  | _ => Form.Expansion.get(sort, t)
  };
};

/* Determine the effective sort for insertion, considering both local and parent sorts.
   Default: local-first (try local sort, fall back to parent).
   Special cases:
   - Semicolon with Mod parent prefers Mod (for ModSeq over CellJoin)
   - Mod context falls back to Exp since bare expressions are valid module items */
let effective_sort = (t: Token.t, z: t, ~root): Sort.t => {
  let local_sort = Relatives.sort(~root, z.relatives);
  let parent_sort = Ancestors.sort(root, z.relatives.ancestors);

  /* Special case: semicolon inside module/sig context should be ModSeq/SigSeq, not CellJoin */
  if (t == ";" && (parent_sort == Sort.Mod || parent_sort == Sort.Sig)) {
    parent_sort;
  } else {
    /* Default: local-first with parent fallback */
    switch (Form.Expansion.try_get(local_sort, t)) {
    | Some(_) => local_sort
    | None =>
      /* In Mod context, try Exp since bare expressions are valid module items.
         This mirrors remold_mod which also falls back to Exp. */
      if (local_sort == Sort.Mod) {
        switch (Form.Expansion.try_get(Exp, t)) {
        | Some(_) => Exp
        | None => parent_sort
        };
      } else {
        parent_sort;
      }
    };
  };
};

/* Shared core for insert_shard and insert_shard_inplace.
 * The only difference is the put_down function used. */
let insert_shard_core =
    (~put_down: (Segment.t, t) => t, ~id: Id.t, t: Token.t, z: t, ~root): t => {
  let z = destroy_selection(z);
  if (Token.is_secondary(t)) {
    put_down([Piece.mk_secondary(id, t)], z);
  } else {
    let sort = effective_sort(t, z, ~root);
    let (label, delim_d) = expansion(sort, t, z);
    let mold = Form.Molds.get(sort, label);
    let shard =
      Tile.split_shards(id, label, mold, List.mapi((i, _) => i, label))
      |> (delim_d == Right ? ListUtil.last : List.hd);
    put_down([Tile(shard)], z);
  };
};

/* `type T¦` in a signature followed by `=`: the bare abstract-member tile
   (`["type"]`, body TPat) becomes shard 0 of `type _ = _`. Its missing `=`
   is then the backpack head (the backpack is derived from incomplete tiles,
   closest first), so the ordinary put-down and reassembly yield
   `type T = ¦` with T as the child. Only the left siblings are touched. */
let upgrade_bare_sig_type = (z: t): option(t) => {
  let (l, r) = z.relatives.siblings;
  let rec find = (~seen_operand, acc, rev) =>
    switch (rev) {
    | [(Piece.Secondary(_) | Grout(_)) as p, ...rest] =>
      find(~seen_operand, [p, ...acc], rest)
    | [Tile({label: [_], shards: [0], mold, _}) as p, ...rest]
        when !seen_operand && (mold.out == Sort.TPat || mold.out == Any) =>
      find(~seen_operand=true, [p, ...acc], rest)
    | [Tile({label: ["type"], mold: {out: Sig, _}, _} as t), ...rest] =>
      Some((List.rev(rest), t, acc))
    | _ => None
    };
  switch (find(~seen_operand=false, [], List.rev(l))) {
  | None => None
  | Some((prefix, t, operand)) =>
    let f = Form.get(SigType);
    let t' =
      Tile.{
        ...t,
        label: f.label,
        mold: f.mold,
        shards: [0],
        children: [],
      };
    Some({
      ...z,
      relatives: {
        ...z.relatives,
        siblings: (prefix @ [Piece.Tile(t'), ...operand], r),
      },
    });
  };
};

/* Insert a new shard based on token `t` on the `d`-side of the caret */
let insert_shard = (~id: Id.t, ~d: Direction.t, t: Token.t, z: t, ~root): t => {
  let z = t == "=" ? Option.value(upgrade_bare_sig_type(z), ~default=z) : z;
  if (Zipper.backpack_find(t, z) != None) {
    let z = destroy_selection(z);
    let target = Zipper.backpack_find(t, z) |> Option.get;
    Zipper.put_down_target(d, target, z, ~root);
  } else {
    insert_shard_core(~put_down=Zipper.put_down_seg(d), ~id, t, z, ~root);
  };
};

/* Replace `d`-neighbor shard with a new one based on token `t` */
let replace_shard = (d: Direction.t, t: Token.t, z: t, ~root): option(t) => {
  let id = Zipper.adjacent_monotile_or_new_id(d, z);
  let+ z = delete(d, z);
  insert_shard(~id, ~d, t, z, ~root);
};

/* Like insert_shard but uses put_down_no_reassemble (no adj_pos,
 * no reassembly). For Inner caret edits where adj_pos would flatten
 * ancestors and reassembly would absorb the token back. */
let insert_shard_inplace = (~id: Id.t, t: Token.t, z: t, ~root): t =>
  insert_shard_core(
    ~put_down=Zipper.put_down_no_reassemble,
    ~id,
    t,
    z,
    ~root,
  );

/* Like replace_shard but without cursor position adjustment.
 * Used when caret is Inner — the token is replaced in-place
 * and the caret stays inside the right neighbor.
 * For secondary pieces (comments, whitespace), directly swaps
 * the piece in siblings to avoid reassembly introducing grout. */
let replace_shard_inplace =
    (d: Direction.t, t: Token.t, z: t, ~root): option(t) => {
  let neighbor = Siblings.neighbor(d, z.relatives.siblings);
  switch (neighbor) {
  | Some(Secondary(w)) when Token.is_secondary(t) =>
    /* Direct replacement: swap the secondary piece in siblings */
    let new_piece = Piece.Secondary(Secondary.mk(w.id, t));
    let (l, r) = z.relatives.siblings;
    let siblings =
      switch (d) {
      | Right =>
        switch (r) {
        | [_, ...rest] => (l, [new_piece, ...rest])
        | _ => (l, r)
        }
      | Left =>
        switch (List.rev(l)) {
        | [_, ...rest] => (List.rev([new_piece, ...rest]), r)
        | _ => (l, r)
        }
      };
    Some({
      ...z,
      relatives: {
        ...z.relatives,
        siblings,
      },
    });
  | _ =>
    let id = Zipper.adjacent_monotile_or_new_id(d, z);
    let+ z = delete(d, z);
    insert_shard_inplace(~id, t, z, ~root);
  };
};

/* True unless the caret is at a bare segment edge (no left neighbor, no
 * ancestor). At a bare edge, a token-merge may need the reassemble/rescan
 * that the general insert path provides; everywhere else the merged token
 * has an enclosing tile/form the caret must not escape. */
let keep_caret_inside_on_append = (z: t): bool =>
  Siblings.neighbor(Left, z.relatives.siblings) != None
  || z.relatives.ancestors != [];

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

/* Check if the RIGHT sibling (without disassembly) is a complete
 * multi-shard tile. Only block rightward merges (where a new token
 * would steal the leading delimiter of the complete tile and cause
 * shard theft during rescan). Leftward merges (extending the trailing
 * delimiter) are legitimate editing (e.g., typing after `in` to
 * make `inner`). */
let has_complete_multishard_right_sibling = (z: t): bool =>
  switch (Siblings.neighbor(Right, z.relatives.siblings)) {
  | Some(Tile(t)) => Tile.is_complete(t) && List.length(t.label) > 1
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
        && !parens_edge_case(char, z)
        && !has_complete_multishard_right_sibling(z) =>
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

/* Check if regrout would insert a grout to our left.
 * Returns the grout so we can insert it ourselves and
 * track its ID for later space redemption. */
let grout_for_suppressed_space = (z: t, ~root): option(Grout.t) =>
  switch (
    Siblings.neighbor(
      Left,
      remold_regrout(Right, z, ~root).relatives.siblings,
    )
  ) {
  | Some(Grout(g)) => Some(g)
  | _ => None
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
  let (l, r) = Token.split_nth(t, idx);
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
    switch (Token.space == char ? grout_for_suppressed_space(z, ~root) : None) {
    | Some(g) =>
      Grout.mark_space_owed(g.id);
      Zipper.put_down_seg(Left, [Grout(g)], z);
    | None =>
      z
      |> insert_shard(~id=Id.mk(), ~d=Left, char)
      |> move_into_string_or_comment(char)
    };
  remold_regrout(Right, z, ~root);
};

/* If the caret is precisely between two tokens, which
 * can become a valid token if merged, merge those tokens.
 * Guarded against merging with a complete multi-shard right
 * sibling to prevent disassembly and shard theft. */
let will_merge = (z: t): option((Token.t, Token.t)) =>
  switch (Zipper.neighbor_tokens(z)) {
  | (Some(l), Some(r))
      when
        Token.is_potential_token(Token.append(l, r))
        && z.caret == Outer
        && !has_complete_multishard_right_sibling(z) =>
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

/* Append char to a neighboring token if possible (biasing left, see
 * sibling_appendability), else insert it as a new token. */
let insert_or_append = (char: string, z: t, ~root): option(t) =>
  switch (sibling_appendability(char, z)) {
  | Some((Right, t))
      when
        Zipper.adjacent_monotile_id(Right, z) != None
        && keep_caret_inside_on_append(z) =>
    /* Prepend to a right monotile, keeping the caret Inner(0) inside the
     * merged token. The in-place insert skips adj_pos, whose move(Left)
     * would escape the enclosing tile/form (e.g. length(¦oo) + f). */
    Caret.set(Inner(0), z)
    |> replace_shard_inplace(Right, t, ~root)
    |> Option.map(remold_regrout(Right, ~root))
  | appendability =>
    let z =
      Caret.set(
        switch (appendability) {
        | Some((Right, _)) => Inner(0)
        | None
        | Some((Left, _)) => Outer
        },
        z,
      );
    let+ z_init =
      switch (appendability) {
      | None =>
        let (id, z) = preserve_grout_id(char, z);
        let z =
          switch (Grout.redeem_space(id)) {
          | Some(w) => Zipper.put_down_seg(Left, [Secondary(w)], z)
          | None => z
          };
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

/* === SELECTION WRAPPING ===
 * When the user types an opening delimiter with an active selection,
 * wrap the selection in that delimiter rather than replacing it.
 * Balanced delimiters (parens, brackets, braces) create a wrapping
 * tile with the selection as child. Quote delimiters (double-quote,
 * backtick, hash) serialize the selection to text and create a
 * token or secondary piece. */

let is_opening_delimiter = (char: string): bool =>
  char == "(" || char == "[" || char == "{";

let delimiter_label = (char: string): Label.t =>
  switch (char) {
  | "(" => ["(", ")"]
  | "[" => ["[", "]"]
  | "{" => ["{", "}"]
  | _ => failwith("not a delimiter: " ++ char)
  };

/* Wrap selection in balanced delimiters. Creates the wrapping tile
 * as an ancestor with the content inside, retaining the selection. */
let wrap_balanced = (~deep_reassociate=false, char: string, z: t, ~root): t => {
  /* Sort is read before the remainders move: they are fragments of the
   * tokens already at this position, so the wrapping tile's mold is the
   * one it would get without them. */
  let sort = Relatives.sort(~root, z.relatives);
  /* A char-level selection holds whole boundary pieces; only the selected
   * characters go inside the new tile, the rest stay outside it. */
  let (left_rem, content, right_rem) = Zipper.split_char_selection(z);
  let (left_sibs, right_sibs) = z.relatives.siblings;
  let (left_sibs, right_sibs) = (
    left_sibs @ left_rem,
    right_rem @ right_sibs,
  );
  let label = delimiter_label(char);
  let mold = Form.Molds.get(sort, label);
  let ancestor: Ancestor.t = {
    id: Id.mk(),
    label,
    mold,
    shards: ([0], [1]),
    children: ([], []),
  };
  /* Re-ID incomplete shards in the content whose counterparts remain
   * in the outer siblings. Wrapping places these at different nesting
   * levels, and shared IDs would cause MakeTerm to create duplicate
   * terms, leading to infinite recursion in the elaborator. */
  let outer_ids =
    Segment.incomplete_tiles(left_sibs @ right_sibs)
    |> List.map((t: Tile.t) => t.id);
  let content =
    List.map(
      fun
      | Piece.Tile(t) when !Tile.is_complete(t) && List.mem(t.id, outer_ids) =>
        Piece.Tile({
          ...t,
          id: Id.mk(),
        })
      | p => p,
      content,
    );
  /* Place content as right siblings inside the new ancestor,
   * remold/regrout with empty selection, then re-select */
  let z = {
    ...z,
    caret: Outer,
    selection: Selection.empty,
    relatives: {
      siblings: ([], content),
      ancestors: [
        (ancestor, (left_sibs, right_sibs)),
        ...z.relatives.ancestors,
      ],
    },
  };
  let z = remold_regrout(Right, z, ~root);
  let z = deep_reassociate ? Reassociate.go(z) : z;
  let right = snd(z.relatives.siblings);
  {
    ...z,
    selection: Selection.mk(~focus=Right, right),
    relatives: {
      ...z.relatives,
      siblings: (fst(z.relatives.siblings), []),
    },
  };
};

/* Get the text of a segment for quote wrapping validation */
let segment_text = (content: Segment.t): string =>
  Segment.to_string(
    ~refractor_seg_to_seg=Triggers.refractor_seg_to_seg,
    ~projector_to_segment=Triggers.projector_to_invoke,
    content,
  );

/* Text actually covered by the selection. A char-level selection keeps whole
 * pieces in `selection.content`, with the real boundaries recorded as Inner
 * offsets, so trim the unselected head/tail of the boundary tokens. */
let selected_text = (z: t): string => {
  let content = z.selection.content;
  let text = segment_text(content);
  let (left_offset, right_offset) = Zipper.char_selection_offsets(z);
  let trim = (n: int, s: string) =>
    Token.split_nth(s, max(0, min(n, Token.length(s))));
  let drop_left =
    switch (left_offset, content) {
    | (Some(n), [p, ..._]) when Piece.token_of(p) != None => n + 1
    | _ => 0
    };
  let drop_right =
    switch (right_offset, ListUtil.last_opt(content)) {
    | (Some(n), Some(p)) =>
      switch (Piece.token_of(p)) {
      | Some(tok) => Token.length(tok) - (n + 1)
      | None => 0
      }
    | _ => 0
    };
  let text = snd(trim(drop_left, text));
  fst(trim(Token.length(text) - drop_right, text));
};

/* Check if text is valid for wrapping in the given delimiter.
 * Rejects text containing the delimiter char or newlines. */
let is_valid_quote_content = (delim: string, text: string): bool =>
  !String.contains(text, '\n') && !String.contains(text, delim.[0]);

/* Wrap selection in a quote delimiter (string, label, or comment).
 * Returns None if the text contains invalid characters, causing
 * fallthrough to normal insert behavior (selection replacement). */
let wrap_quote = (char: string, z: t, ~root): option(t) => {
  let text = selected_text(z);
  if (!is_valid_quote_content(char, text)) {
    None;
  } else {
    /* Delete exactly the selected characters. For a char-level selection
     * this splits the boundary tokens and keeps their exterior, leaving the
     * caret Inner at the seam when both halves survive in one token. */
    let z = Zipper.normalize_char_selection(z);
    let z =
      Selection.is_empty(z.selection)
        /* Already normalized away; an Inner caret here is the seam. */
        ? z
        /* Whole-piece selection: an Inner caret left over from the destroyed
         * selection would be misread against whatever ends up on the right. */
        : destroy_selection(z) |> Caret.set(Outer);
    let token =
      Token.is_comment_delim(char)
        ? "#" ++ text ++ "#" : char ++ text ++ char;
    switch (z.caret, Zipper.neighbor_tokens(z)) {
    | (Inner(idx), (_, Some(t))) =>
      /* Seam inside a surviving token: split it around the new one. */
      split(z, token, idx + 1, t, ~root)
    | _ =>
      let piece =
        if (Token.is_comment_delim(char)) {
          Piece.mk_secondary(Id.mk(), token);
        } else {
          let sort = Relatives.sort(~root, z.relatives);
          let mold = Form.Molds.get(sort, [token]);
          Piece.Tile({
            id: Id.mk(),
            label: [token],
            mold,
            shards: [0],
            children: [],
          });
        };
      Some(Zipper.insert_segment(z, [piece], ~root));
    };
  };
};

/* Try to wrap selection in a delimiter. Returns Some if wrapping
 * occurred, None to fall through to normal insert behavior. */
let try_wrap_selection =
    (~deep_reassociate=false, char: string, z: t, ~root): option(t) =>
  if (is_opening_delimiter(char)) {
    Some(wrap_balanced(~deep_reassociate, char, z, ~root));
  } else if (Token.is_string_or_comment_delim(char)) {
    wrap_quote(char, z, ~root);
  } else {
    None;
  };

let go = (~deep_reassociate=false, char: string, z: t, ~root): option(t) => {
  /* If there's a selection, try wrapping before falling through */
  switch (
    z.selection.content != []
      ? try_wrap_selection(~deep_reassociate, char, z, ~root) : None
  ) {
  | Some(z) => Some(z)
  | None =>
    /* Normal path: normalize char selection then delete (if any) */
    let z =
      z.selection.content != []
        ? Zipper.normalize_char_selection(z) |> Zipper.destroy_selection : z;
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
          |> replace_shard_inplace(Right, new_token, ~root)
          |> Option.map(
               Token.is_secondary(new_token)
                 ? Fun.id : remold_regrout(Right, ~root),
             )
        : split(z, char, idx, t, ~root);
    | (Inner(_), (_, None)) => None
    | (Outer, _) => insert_or_append(char, z, ~root)
    };
  };
};

/* This is a wrapper intended to effectuate after-insertion conditional
 * operations. See Triggers.re for more details */
let go =
    (
      ~deep_reassociate=false,
      ~ci: option(Language.Info.t)=None,
      char: string,
      z: t,
      ~root,
    )
    : option(t) => {
  let+ z = go(~deep_reassociate, char, z, ~root);
  let z = Triggers.insert(~ci, z);
  let z =
    switch (z.caret) {
    | Inner(_) => z
    | Outer => Zipper.rescan_reassemble(Left, z, ~root)
    };
  z;
};
