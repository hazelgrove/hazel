open Zipper;
open Util;
open OptUtil.Syntax;

let barf = (d: Direction.t, tok: Token.t, z: t): option(t) => {
  /* Removes the d-neighboring tile and drops from backpack;
     precondition: the d-neighbor should be a monotile
     string-matching the dropping shard */
  let* z = delete(d, z);
  let+ z = put_down_tok(d, tok, z);
  z;
};

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

let delayed_expand = (t: Token.t, caret: Direction.t, z: t): option(t) => {
  /* Removes the d-neighboring tile and reconstructs it, triggering
     keyword-expansion; precondition: the d-neighbor should be a monotile
     string-matching a keyword of an expanding form */
  let (new_label, backpack) = Molds.delayed_expansion(t);
  /* Only expand case rules when inside a case */
  let (new_label, backpack) =
    switch () {
    | () when (before_case_shard(z) || inside_case(z)) && t == "|" => (
        ["|", "=>"],
        Direction.Left,
      )
    | _ when t == "|" => ([t], Direction.Left)
    | _ => (new_label, backpack)
    };
  /* Retain monotile id for new polytile (Just for fun) */
  let id =
    switch (adjacent_monotile_id(caret, z)) {
    | Some(id) => id
    | None => Id.mk()
    };
  let+ z = delete(caret, z);
  construct(~id, ~backpack, ~caret, new_label, z);
};

let expand_or_barf_left_neighbor = (z as s: t): option(t) =>
  /* If left neighbor is a monotile (a) string-matching the shard at the
     top of the backpack, barf it, or (b) an expansing keyword, expand it. */
  switch (left_neighbor_monotile(z.relatives.siblings)) {
  | Some(t) when Zipper.will_barf(t, z) => barf(Left, t, s)
  | Some(t) when Molds.is_delayed(t) => delayed_expand(t, Left, s)
  | _ => Some(s)
  };

let expand_or_barf_right_neighbor = (z as s: t): option(t) =>
  /* If right neighbor is a monotile (a) string-matching the shard at the
     top of the backpack, barf it, or (b) an expansing keyword, expand it. */
  switch (right_neighbor_monotile(z.relatives.siblings)) {
  | Some(t) when Zipper.will_barf(t, z) => barf(Right, t, s)
  | Some(t) when Molds.is_delayed(t) => delayed_expand(t, Right, s)
  | _ => Some(s)
  };

let get_duo_shard = ({label, shards, _}: Tile.t) =>
  if (List.length(label) == 2 && List.length(shards) == 1) {
    List.nth_opt(label, List.hd(shards));
  } else {
    None;
  };

let neighbor_can_duomerge =
    (t: Token.t, s: Siblings.t): option((Label.t, Direction.t, Id.t)) =>
  /* Checks if a neighbor, preferentially the left neighbor, is
     a shard of a duotile which can be merged to form a monotile.
     It returns the resulting (mono)label, and the direction of
     the relevant neighbor. */
  switch (Siblings.neighbors(s)) {
  | (Some(Tile(tile)), _) =>
    let* start = get_duo_shard(tile);
    let+ mono_lbl = Form.duomerges([start, t]);
    (mono_lbl, Direction.Left, tile.id);
  | (_, Some(Tile(tile))) =>
    let* last = get_duo_shard(tile);
    let+ mono_lbl = Form.duomerges([t, last]);
    (mono_lbl, Direction.Right, tile.id);
  | _ => None
  };

let make_new_tile = (~id, t: Token.t, caret: Direction.t, z: t): t =>
  /* Adds a new tile at the caret. If the new token matches the top
     of the backpack, the backpack shard is dropped. Otherwise, we
     construct a new tile, which may immediately expand. */
  switch (neighbor_can_duomerge(t, z.relatives.siblings)) {
  | Some((lbl, d, id)) =>
    Zipper.replace(~id, ~caret=d, ~backpack=d, lbl, z) |> Option.get
  | None =>
    /* e.g. closing parens are put down without further ceremony */
    Zipper.will_barf(t, z) && Form.is_instant_putdown(t)
      ? put_down_regrout_remold_tok(caret, t, z) |> Option.get
      : {
        let (lbl, backpack) = Molds.instant_expansion(t);
        construct(~id, ~caret, ~backpack, lbl, z);
      }
  };

let expand_neighbors_and_make_new_tile = (char: Token.t, state: t): option(t) => {
  /* Trigger a token boundary event and create a new tile.
     This process potentially involves both neighboring tiles,
     potentially triggering up to 3 expansions or backpack barfs.
     In particular, both left and right neighboring monotiles may
     undergo delayed (aka keyword) expansion, and the newly-created
     single-character token may undergo instant expansion. Currently
     made the decision to expand or barf the neighbors before making
     the new tile because barfing is limited to the top of the backpack,
     and I wanted things like "if|then", when you enter a "(", to
     barf the "then", before it is buried by the ")" added to the BP.
     The order here could be revisited if barfing was more sophisticated.
     */
  let* z = expand_or_barf_left_neighbor(state);
  let+ z = expand_or_barf_right_neighbor(z);
  let z = remold_regrout_prev(z);
  let z = make_new_tile(~id=Id.mk(), char, Left, z);
  let z = remold_regrout_prev(z);
  z;
};

let replace_tile = (t: Token.t, d: Direction.t, z: t): option(t) => {
  let id =
    switch (adjacent_monotile_id(d, z)) {
    | Some(id) => id
    | None => Id.mk()
    };
  let+ z = delete(d, z);
  make_new_tile(~id, t, d, z);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type appendability =
  | AppendLeft(Token.t)
  | AppendRight(Token.t)
  | MakeNew;

let sibling_appendability: (string, Siblings.t) => appendability =
  (char, siblings) =>
    switch (neighbor_monotiles(siblings)) {
    | (Some(t), _) when Molds.allow_append_right(t, char) =>
      AppendLeft(t ++ char)
    | (_, Some(t)) when Molds.allow_append_left(char, t) =>
      AppendRight(char ++ t)
    | _ => MakeNew
    };

let insert_outer = (char: string, z as state: t): option(t) =>
  switch (sibling_appendability(char, z.relatives.siblings)) {
  | MakeNew => expand_neighbors_and_make_new_tile(char, state)
  | AppendLeft(t) => replace_tile(t, Left, state)
  | AppendRight(t) => replace_tile(t, Right, state)
  };

let insert_duo = (lbl: Label.t, z: option(t)): option(t) =>
  z
  |> Option.map(z => Zipper.construct(~caret=Left, ~backpack=Left, lbl, z))
  |> OptUtil.and_then(z => {
       //NOTE: regrout to put e.g. ap(1|) back together
       z
       |> remold_regrout(Left)
       |> Zipper.put_down_tok(Left, List.nth(lbl, 1))
       |> OptUtil.and_then(Zipper.move(Left))
     });

let insert_monos = (~id, l: Token.t, r: Token.t, z: option(t)): option(t) =>
  z
  |> Option.map(Zipper.construct_mono(~id=Id.mk(), Right, r))
  |> Option.map(Zipper.construct_mono(~id, Left, l));

let should_supress_space = (z: t): bool => {
  /* Figure out if we should avoid inserting a space because a grout
   * is due to be inserted instead */
  let z_cand = z |> remold_regrout(Right);
  let init_left_nhbr = Siblings.left_neighbor(z.relatives.siblings);
  let candidate_nhbr = Siblings.left_neighbor(z_cand.relatives.siblings);
  switch (Siblings.left_neighbor(z_cand.relatives.siblings)) {
  | None => false
  | Some(p) => Piece.is_grout(p) && candidate_nhbr != init_left_nhbr
  };
};

let move_into_if_stringlit_or_comment = (char, z) =>
  /* This is special-case logic for advancing the caret to position between the quotes
     in newly-created stringlits. The main stringlit special-case is in Zipper.constuct
     and ideally this logic would be located there as well, but both regrouting and
     subsequent caret position logic at this function's callsites dicate that this
     be done after. Not too happy about this tbh. */
  Form.is_string_delim(char) || Form.is_comment_delim(char)
    ? switch (move(Left, z)) {
      | None => z
      | Some(z) => z |> set_caret(Inner(0, 0))
      }
    : z;

let split = (z: t, char: string, idx: int, t: Token.t): option(t) => {
  /* Current this necessarily creates three tokens; two from splitting
   * the existing one, and a new one. The two splitting tokens may become
   * delimiters of the same time (e.g. `[|]`=>`[<>|]`). In the future it
   * may be prudent to relax this by, after splitting, first attempting
   * to append the new char to the left half, and then the right half,
   * and only if those fail creating a new center token. */
  let (l, r) = Token.split_nth(idx, t);
  let right_monotile_id =
    switch (adjacent_monotile_id(Right, z)) {
    | Some(id) => id
    | None => Id.mk()
    };
  /* overwrite selection */
  let z = z |> Zipper.set_caret(Outer) |> Zipper.select(Right);
  switch (Form.duomerges([l, r])) {
  | Some(_) =>
    let+ z = insert_duo([l, r], z);
    /* If we're inserting a space, don't bother to insert it;
     * we'll get a convex grout anyway from regrouting */

    (Form.space != char ? make_new_tile(~id=Id.mk(), char, Left, z) : z)
    |> remold_regrout(Right)
    |> move_into_if_stringlit_or_comment(char);

  | None =>
    /* If contemplating changing regrouting behavior here, try these
     * two cases: pressing (A) space and (B) open parens on:
     * `if then|else` (needs convex grout in prev seg and current seg by caret)
     * `if true|then` (no grout needed by caret, and later)
     * `if|then` (needs convex grout by caret in current seg)
     * `1|1` (needs concave grout by caret in current seg)
     * `if|if` (no grout needed by caret, and later)
     * `case|end` */
    let* z = insert_monos(~id=right_monotile_id, l, r, z);
    let* z = expand_or_barf_left_neighbor(z);
    let+ z = expand_or_barf_right_neighbor(z);
    if (Form.space == char && should_supress_space(z)) {
      /* This is a finnicky case. remold_regrout_prev regrouts
       * the parent segment if we're at the beginning of the current
       * segment, but that also causes it to regrout the current
       * segment, which may result in us ending up on the wrong
       * side of the grout */
      let z = z |> remold_regrout_prev |> remold_regrout(Left);
      switch (move(Right, z)) {
      | None => z
      | Some(z) => z
      };
    } else {
      let z = remold(z);
      let z =
        z |> remold_regrout_prev |> make_new_tile(~id=Id.mk(), char, Left);
      let z = z |> remold_regrout(Right);
      let z = z |> move_into_if_stringlit_or_comment(char);
      z;
    };
  };
};

let closing_stringlit_or_comment = (char, t) =>
  Form.is_string(t)
  && Form.is_string_delim(char)
  || Form.is_comment(t)
  && Form.is_comment_delim(char);

let invoked_projector = (name: string, syntax: Segment.t): option(Piece.t) => {
  let* name = Form.of_projector_invoke(name);
  let kind = ProjectorCore.Kind.of_name(name);
  ProjectorPerform.init(kind, syntax);
};

let is_projector_invoke = (z: t): option(t) => {
  switch (z.relatives.siblings |> fst |> List.rev) {
  | [
      Tile({label: ["(", ")"], children: [syntax], _}),
      Tile({label: [name], _}),
      ...rest,
    ]
      when Form.is_projector_invoke(name) =>
    /* Trim only need because of grout/whitespace transmutation when syntax is hole */
    let syntax =
      syntax |> Segment.trim_secondary(Right) |> Segment.trim_secondary(Left);
    let+ piece = invoked_projector(name, syntax);
    Zipper.update_siblings(
      ((_, r)) => ([piece, ...rest] |> List.rev, r),
      z,
    );
  /* Special case for reparsing of projectors placed on holes */
  | [Tile({label: ["()"], _}), Tile({label: [name], _}), ...rest]
      when Form.is_projector_invoke(name) =>
    let+ piece = invoked_projector(name, [Piece.mk_grout(Convex)]);
    Zipper.update_siblings(
      ((_, r)) => ([piece, ...rest] |> List.rev, r),
      z,
    );
  | _ => None
  };
};

let projector_to_invoke: Base.projector => Segment.t =
  pr => [
    Piece.mk_tile(
      Form.mk(
        Form.ss,
        [Form.mk_projector_invoke(pr.kind)],
        Mold.(mk_op(Exp, [])),
      ),
      [],
    ),
    Piece.mk_tile(Form.get(ApExp), [Piece.unparenthesize(pr.syntax)]),
  ];

let rec go =
        (
          ~ctx: option(Language.Ctx.t)=?,
          char: string,
          {caret, relatives: {siblings, _}, _} as z: t,
        )
        : option(t) => {
  /* If there's a selection, delete it before proceeding */
  let z = z.selection.content != [] ? Zipper.destruct(z) : z;
  switch (caret, neighbor_monotiles(siblings)) {
  /* If we try to insert a quote inside an existing string, or a #
   * in a comment, we are instead moved to the righthand side of
   * the operand. Note that this behavior is load-bearing for the
   * current parsing approach including Paste */
  | (_, (_, Some(t))) when closing_stringlit_or_comment(char, t) =>
    z |> Zipper.set_caret(Outer) |> Zipper.move(Right)
  | (Outer, (Some(t), _)) when closing_stringlit_or_comment(char, t) =>
    Some(z)
  | (Outer, (Some(t), _)) when Form.is_livelit(t) && char == " " =>
    let insert = (z, c) => Option.bind(z, go(c));
    switch (ctx) {
    | Some(ctx) =>
      let name = Form.parse_livelit(t);
      switch (Language.Ctx.lookup_livelit(ctx, name)) {
      // if we find a matching livelit, insert it, projected
      | Some(ll) =>
        let exp_to_segment =
          ExpToSegment.(
            exp_to_segment(
              ~settings=
                Settings.of_core(~inline=true, Language.CoreSettings.on),
            )
          );

        let model_segment =
          switch (ll.model_default) {
          | {term: Tuple(_), _} => ll.model_default |> exp_to_segment
          | _ => [Segment.parenthesize(ll.model_default |> exp_to_segment)]
          };

        let model_zipper =
          model_segment
          |> Segment.to_string(~projector_to_segment=(p: Base.projector) =>
               Base.unparenthesize(p.syntax)
             )
          |> StringUtil.to_list
          |> List.fold_left(insert, Some(z));

        let args_and_name =
          switch (model_zipper) {
          | Some(z) =>
            Some(z.relatives.siblings |> fst |> List.rev |> ListUtil.take(2))
          | None => None
          };

        let updated_syntax =
          ProjectorInit.init_or_noop(
            Livelit,
            Segment.parenthesize(Option.get(args_and_name)),
            MakeTerm.for_projection(Option.get(args_and_name)) |> Option.get,
          );

        let new_left_siblings =
          switch (List.rev(fst(z.relatives.siblings))) {
          | [_hd, ...tl] => List.rev([updated_syntax, ...tl])
          | [] => []
          };

        Some(
          Option.get(model_zipper)
          |> Zipper.update_siblings(((_, r)) => (new_left_siblings, r)),
        );

      // No matching livelit found, insert space
      | None => insert_outer(char, z)
      };
    | None => insert(Some(z), char)
    };
  | (Inner(d_idx, n), (_, Some(t))) =>
    let idx = n + 1;
    let new_t = Token.insert_nth(idx, char, t);
    /* If inserting wouldn't produce a valid token, split. This is
     * mostly targetting the case of inserting an infix operator
     * inside an operand (or more rarely vice-versa). In such cases,
     * due to the current MOSTLY disjointedness of these character
     * classes, ALL (ish?) current splits should be 3-way
     * splits (as opposed to 2-way). This is currently the only
     * kind of splitting supported; this should be revisited if
     * we move to more subtle token division logic */
    Molds.allow_insertion(char, t, new_t)
      ? z
        |> Zipper.set_caret(Inner(d_idx, idx))
        |> Zipper.replace_mono(Right, new_t)
        |> Option.map(remold_regrout(Left))
      : split(z, char, idx, t);
  /* Can't insert inside delimiter */
  | (Inner(_, _), (_, None)) => None
  | (Outer, (_, Some(_))) =>
    let caret: Zipper.Caret.t =
      switch (sibling_appendability(char, siblings)) {
      | AppendRight(_) =>
        /* If we're adding to the right, move caret inside right nhbr.
         * Note the assumption that this is a monotile */
        Inner(0, 0)
      | MakeNew
      | AppendLeft(_) => Outer
      };
    z
    |> insert_outer(char)
    |> Option.map(Zipper.set_caret(caret))
    |> Option.map(remold_regrout(Left))
    |> Option.map(move_into_if_stringlit_or_comment(char));
  | (Outer, (_, None)) =>
    z
    |> insert_outer(char)
    |> Option.map(remold_regrout(Left))
    |> Option.map(move_into_if_stringlit_or_comment(char))
  };
};

let go = (~ctx: option(Language.Ctx.t)=?, char: string, z: t): option(t) => {
  /* This is a wrapper intended to effectuate after-insertion conditional
   * operations. This is done here as opposed to in perform in order to
   * reflect operations we want performed by the parser, which uses
   * Insert.go as its primary driver */
  let+ z = go(~ctx?, char, z);
  switch (is_projector_invoke(z)) {
  | Some(z) => z
  | None => z
  };
};
