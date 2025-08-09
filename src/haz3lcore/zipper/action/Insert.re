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

let expand = (t: Token.t, caret: Direction.t, z: t): option(t) => {
  /* Removes the d-neighboring tile and reconstructs it, triggering
     keyword-expansion; precondition: the d-neighbor should be a monotile
     string-matching a keyword of an expanding form */
  let (new_label, backpack) = Form.Expansion.get(t);
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

let expand_or_barf_left_neighbor = (z: t): t =>
  /* If left neighbor is a monotile (a) string-matching the shard at the
     top of the backpack, barf it, or (b) an expansing keyword, expand it. */
  switch (Zipper.left_neighbor_shard(z)) {
  | Some(t) when Zipper.will_barf(t, z) =>
    switch (barf(Left, t, z)) {
    | Some(z) => z
    | None => z
    }
  | Some(t) when Form.Expansion.will(t) =>
    switch (Siblings.left_neighbor(z.relatives.siblings)) {
    | Some(p) when Piece.monotile(p) != None =>
      switch (expand(t, Left, z)) {
      | Some(z) => z
      | None => z
      }
    | _ => z
    }
  | _ => z
  };

let expand_or_barf_right_neighbor = (z: t): t =>
  /* If right neighbor is a monotile (a) string-matching the shard at the
     top of the backpack, barf it, or (b) an expansing keyword, expand it. */
  switch (Zipper.right_neighbor_shard(z)) {
  | Some(t) when Zipper.will_barf(t, z) =>
    switch (barf(Right, t, z)) {
    | Some(z) => z
    | None => z
    }
  | Some(t) when Form.Expansion.will(t) =>
    switch (Siblings.right_neighbor(z.relatives.siblings)) {
    | Some(p) when Piece.monotile(p) != None =>
      switch (expand(t, Right, z)) {
      | Some(z) => z
      | None => z
      }
    | _ => z
    }
  | _ => z
  };

let expand_or_barf_neighbors = (z: t): t => {
  let z = expand_or_barf_left_neighbor(z);
  let z = expand_or_barf_right_neighbor(z);
  z;
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
    let+ mono_lbl = Token.duomerges([start, t]);
    (mono_lbl, Direction.Left, tile.id);
  | (_, Some(Tile(tile))) =>
    let* last = get_duo_shard(tile);
    let+ mono_lbl = Token.duomerges([t, last]);
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
    //TODO(andrew): rm instant putdown if no longer necessary
    Zipper.will_barf(t, z)
      /*&& Form.is_instant_putdown(t)*/
      ? put_down_regrout_remold_tok(caret, t, z) |> Option.get
      : {
        let (lbl, backpack) = Form.Expansion.get(t);
        //TODO(andrew): fix hack
        let (lbl, backpack) =
          if (List.length(lbl) == 1) {
            let (new_label, backpack) =
              //copy-pasted from `expand`
              //TODO(andrew): copy over other stuff from above like id retention?
              switch () {
              | () when (before_case_shard(z) || inside_case(z)) && t == "|" => (
                  ["|", "=>"],
                  Direction.Left,
                )
              | _ when t == "|" => ([t], Direction.Left)
              | _ => (lbl, backpack)
              };
            (new_label, backpack);
          } else {
            (lbl, backpack);
          };
        construct(~id, ~caret, ~backpack, lbl, z);
      }
  };

let replace_shard = (d: Direction.t, t: Token.t, z: t): option(t) => {
  let id =
    switch (adjacent_monotile_id(d, z)) {
    | Some(id) => id
    | None => Id.mk()
    };
  let+ z = delete(d, z);
  make_new_tile(~id, t, d, z);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type appendability = option((Direction.t, Token.t));

let sibling_appendability: (string, t) => appendability =
  (char, z) =>
    switch (neighbor_shards(z)) {
    | (Some(t), _) when Token.is_potential_token(Token.append(t, char)) =>
      Some((Left, Token.append(t, char)))
    | (_, Some(t)) when Token.is_potential_token(Token.append(char, t)) =>
      Some((Right, Token.append(char, t)))
    | _ => None
    };

let insert_outer = (char: string, z: t): option(t) =>
  switch (sibling_appendability(char, z)) {
  | None =>
    /* Trigger a token boundary event and create a new tile.
       This process potentially involves both neighboring tiles,
       potentially triggering up to 3 expansions or backpack barfs.
       In particular, both left and right neighboring monotiles may
       undergo expansion, and the newly-created
       single-character token may undergo expansion. Currently
       made the decision to expand or barf the neighbors before making
       the new tile because barfing is limited to the top of the backpack,
       and I wanted things like "if|then", when you enter a "(", to
       barf the "then", before it is buried by the ")" added to the BP.
       The order here could be revisited if barfing was more sophisticated.
       */
    z
    |> expand_or_barf_neighbors
    |> make_new_tile(~id=Id.mk(), char, Left)
    |> Option.some
  | Some((Left, t)) => replace_shard(Left, t, z)
  | Some((Right, t)) => replace_shard(Right, t, z)
  };

let insert_duo = (~id, lbl: Label.t, z: option(t)): option(t) =>
  z
  |> Option.map(z =>
       Zipper.construct(~id, ~caret=Left, ~backpack=Left, lbl, z)
     )
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

let should_supress_space = (z: t): bool =>
  /* Figure out if we should avoid inserting a space because a grout
   * is due to be inserted instead */
  switch (
    Siblings.left_neighbor(remold_regrout(Right, z).relatives.siblings)
  ) {
  | None => false
  | Some(p) => Piece.is_grout(p)
  };

let move_into_if_stringlit_or_comment = (char, z) =>
  /* This is special-case logic for advancing the caret to position between the quotes
     in newly-created stringlits. The main stringlit special-case is in Zipper.constuct
     and ideally this logic would be located there as well, but both regrouting and
     subsequent caret position logic at this function's callsites dicate that this
     be done after. Not too happy about this tbh. */
  Token.is_string_delim(char) || Token.is_comment_delim(char)
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
  switch (Token.duomerges([l, r])) {
  | Some(_) =>
    let+ z = insert_duo(~id=right_monotile_id, [l, r], z);
    /* If we're inserting a space, don't bother to insert it;
     * we'll get a convex grout anyway from regrouting */

    (Token.space != char ? make_new_tile(~id=Id.mk(), char, Left, z) : z)
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
    let+ z = insert_monos(~id=right_monotile_id, l, r, z);
    let z = expand_or_barf_neighbors(z);
    if (Token.space == char && should_supress_space(z)) {
      /* This is a finnicky case. remold_regrout_prev regrouts
       * the parent segment if we're at the beginning of the current
       * segment, but that also causes it to regrout the current
       * segment, which may result in us ending up on the wrong
       * side of the grout */
      let z = z /*|> remold_regrout_prev*/ |> remold_regrout(Right);
      // switch (move(Right, z)) {
      // | None => z
      // | Some(z) => z
      // };
      z;
    } else {
      let z = remold(z);
      let z = z |> make_new_tile(~id=Id.mk(), char, Left);
      let z = z |> move_into_if_stringlit_or_comment(char);
      let z = z |> remold_regrout(Right);
      z;
    };
  };
};

let invoked_projector = (name: string, syntax: Segment.t): option(Piece.t) => {
  let* name = Token.of_projector_invoke(name);
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
      when Token.is_projector_invoke(name) =>
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
      when Token.is_projector_invoke(name) =>
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
      Form.mk_atom_op(Exp, Token.mk_projector_invoke(pr.kind)),
      [],
    ),
    Piece.mk_tile(Form.get(ApExp), [Piece.unparenthesize(pr.syntax)]),
  ];

let expand_livelit = (z: t, insert, ll: Language.LivelitCtx.raw_livelit) => {
  let seg =
    ExpToSegment.exp_to_segment(
      ~settings=
        ExpToSegment.Settings.of_core(~inline=true, Language.CoreSettings.on),
      ll.model_default,
    );
  let* z =
    (
      switch (ll.model_default) {
      | {term: Tuple(_), _} => seg
      | _ => [Segment.parenthesize(seg)]
      }
    )
    |> Segment.to_string(~projector_to_segment=p =>
         Base.unparenthesize(p.syntax)
       )
    |> Token.to_list
    |> List.fold_left(insert, Some(z));
  let args_and_name =
    z.relatives.siblings |> fst |> List.rev |> ListUtil.take(2);
  let+ any = MakeTerm.for_projection(args_and_name);
  let proj =
    ProjectorInit.init_or_noop(
      Livelit,
      Segment.parenthesize(args_and_name),
      any,
    );
  Zipper.update_siblings(
    ((l, r)) => (fst(ListUtil.split_last(l)) @ [proj], r),
    z,
  );
};

let rec go = (~ctx: option(Language.Ctx.t)=?, char: string, z: t): option(t) => {
  /* If there's a selection, delete it before proceeding */
  let z = z.selection.content != [] ? Zipper.destruct(z) : z;
  switch (z.caret, neighbor_shards(z)) {
  /* If we try to insert a quote inside an existing string, or a #
   * in a comment, we are instead moved to the righthand side of
   * the operand. Note that this behavior is load-bearing for the
   * current parsing approach including Paste */
  | (_, (_, Some(t))) when Token.closing_stringlit_or_comment(char, t) =>
    z |> Zipper.set_caret(Outer) |> Zipper.move(Right)
  | (Outer, (Some(t), _)) when Token.closing_stringlit_or_comment(char, t) =>
    Some(z)
  | (Outer, (Some(t), _)) when Token.is_livelit(t) && char == " " =>
    let insert = (z, c) => Option.bind(z, go(c));
    switch (ctx) {
    | Some(ctx) =>
      switch (Language.Ctx.lookup_livelit(ctx, Token.parse_livelit(t))) {
      // if we find a matching livelit, insert it, projected
      | Some(ll) => expand_livelit(z, insert, ll)
      // No matching livelit found, insert space
      | None => insert_outer(char, z)
      }
    | None => insert(Some(z), char)
    };
  | (Inner(_, n), (_, Some(t))) =>
    let idx = n + 1;
    let new_t = Token.insert_nth(idx, char, t);
    /* Even if we weren't on delim 0 before, we will be after as the
     * insertion will break the polytile, leaving us on a monotile. */
    let z = Zipper.set_caret(Inner(0, idx), z);
    /* If inserting wouldn't produce a valid token, split. This is
     * mostly targetting the case of inserting an infix operator
     * inside an operand (or more rarely vice-versa). In such cases,
     * due to the current MOSTLY disjointedness of these character
     * classes, ALL (ish?) current splits should be 3-way
     * splits (as opposed to 2-way). This is currently the only
     * kind of splitting supported; this should be revisited if
     * we move to more subtle token division logic */
    Token.is_potential_token(new_t)
      ? {
        z
        |> replace_shard(Right, new_t)
        |> Option.map(Zipper.set_caret(Inner(0, idx)))  /* Always 0 delim after */
        |> Option.map(remold_regrout(Right));
      }
      : split(z, char, idx, t);
  /* Can't insert inside delimiter */
  | (Inner(_, _), (_, None)) => None
  | (Outer, (_, Some(_))) =>
    z
    |> insert_outer(char)
    |> Option.map(
         Zipper.set_caret(
           switch (sibling_appendability(char, z)) {
           | Some((Right, _)) =>
             /* If we're adding to the right, move caret inside right nhbr.
              * TODO(andrew): monotile assumption */
             Inner(0, 0)
           | None
           | Some((Left, _)) => Outer
           },
         ),
       )
    |> Option.map(remold_regrout(Right))
    |> Option.map(move_into_if_stringlit_or_comment(char))
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
