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
  /* Only expand case rules when inside a case */
  | "|" when !(before_case_shard(z) || inside_case(z)) => ([t], Left)
  | _ => Form.Expansion.get(t)
  };
};

let insert_token =
    (
      ~id: Id.t,
      ~d: Direction.t, /* Caret-relative direction for insertion of new piece */
      t: Token.t,
      z: t,
    )
    : t => {
  let z = destroy_selection(z);
  let (label, backpack) = expansion(t, z);
  switch (label) {
  | [content] when Token.is_secondary(content) =>
    Zipper.put_down_seg(
      d,
      Token.is_comment(content)
        ? Base.mk_secondary(id, Secondary.construct_comment(content))
        : Base.mk_secondary(id, Whitespace(content)),
      z,
    )
  | _ =>
    let label =
      switch (label) {
      | [t] when Token.is_string_delim(t) =>
        /* Special case for constructing string literals. */
        [Token.string_delim ++ Token.string_delim]
      | _ => label
      };
    let molds = Form.Molds.get(label);
    assert(molds != []);
    // initial mold to typecheck, will be remolded
    let mold = List.hd(molds);
    let shard =
      Tile.split_shards(id, label, mold, List.mapi((i, _) => i, label))
      |> List.map(Segment.of_tile)
      |> ListUtil.rev_if(backpack == Right)
      |> List.hd;
    Zipper.put_down_seg(d, shard, z);
  };
};

let new_shard = (~id: Id.t, ~d: Direction.t, t: Token.t, z: t): t =>
  /* Adds a new tile at the caret. If the new token matches the top
     of the backpack, the backpack shard is dropped. Otherwise, we
     construct a new tile, which may immediately expand. */
  Zipper.will_barf(t, z)
    ? put_down_regrout_remold_tok(d, t, z) |> Option.get
    : insert_token(~id, ~d, t, z);

let replace_expand = (d: Direction.t, t: Token.t, z: t): option(t) => {
  /* Retain monotile id for new polytile (Just for fun) */
  let id = Zipper.adjacent_monotile_or_new_id(d, z);
  let+ z = delete(d, z);
  insert_token(~id, ~d, t, z);
};

let replace_shard = (d: Direction.t, t: Token.t, z: t): option(t) => {
  let id = Zipper.adjacent_monotile_or_new_id(d, z);
  let+ z = delete(d, z);
  new_shard(~id, ~d, t, z);
};

/* Removes a neighboring shard and drops from backpack;
   Precondition: the d-neighbor should be a shard
   string-matching the dropping shard */
let barf = (d: Direction.t, t: Token.t, z: t): option(t) => {
  let* z = delete(d, z);
  put_down_tok(d, t, z);
};

let barf_or_noop = (d: Direction.t, t: Token.t, z: t): t =>
  switch (barf(d, t, z)) {
  | Some(z) => z
  | None => z
  };

let expand_or_noop = (d: Direction.t, t: Token.t, z: t): t =>
  switch (replace_expand(d, t, z)) {
  | Some(z) => z
  | None => z
  };

/* If left neighbor is a monotile (a) string-matching the shard at the
   top of the backpack, barf it, or (b) an expanding keyword, expand it. */
let expand_or_barf_left_neighbor = (z: t): t =>
  switch (Zipper.neighbor_shard(Left, z)) {
  | Some(t) when Zipper.will_barf(t, z) => barf_or_noop(Left, t, z)
  | Some(t) when Form.Expansion.will(t) =>
    switch (Siblings.left_neighbor(z.relatives.siblings)) {
    | Some(p) when Piece.monotile(p) != None => expand_or_noop(Left, t, z)
    | _ => z
    }
  | _ => z
  };

/* If right neighbor is a monotile (a) string-matching the shard at the
   top of the backpack, barf it, or (b) an expanding delimiter, expand it. */
let expand_or_barf_right_neighbor = (z: t): t =>
  switch (Zipper.neighbor_shard(Right, z)) {
  | Some(t) when Zipper.will_barf(t, z) => barf_or_noop(Right, t, z)
  | Some(t) when Form.Expansion.will(t) =>
    switch (Siblings.right_neighbor(z.relatives.siblings)) {
    | Some(p) when Piece.monotile(p) != None => expand_or_noop(Right, t, z)
    | _ => z
    }
  | _ => z
  };

let expand_or_barf_neighbors = (z: t): t =>
  z |> expand_or_barf_left_neighbor |> expand_or_barf_right_neighbor;

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

let append_or_construct = (char: string, z: t): option(t) =>
  switch (sibling_appendability(char, z)) {
  | None =>
    z
    |> expand_or_barf_neighbors
    |> new_shard(~id=Id.mk(), ~d=Left, char)
    |> Option.some
  | Some((d, t)) => replace_shard(d, t, z)
  };

/* Figure out if we should avoid inserting a space
 * because grout is due to be inserted instead */
let should_supress_space = (z: t): bool =>
  switch (
    Siblings.left_neighbor(remold_regrout(Right, z).relatives.siblings)
  ) {
  | None => false
  | Some(p) => Piece.is_grout(p)
  };

/* This is special-case logic for advancing the caret to between
 * the quotes in newly-created stringlits. The bulk of the stringlit
 * special-casing is in Zipper.constuct; ideally this logic would be
 * there as well, but both regrouting and subsequent caret logic at
 * this function's callsites require this be done after :( */
let move_into_string_or_comment = (char: string, z: t): t =>
  Token.is_string_delim(char) || Token.is_comment_delim(char)
    ? switch (move(Left, z)) {
      | None => z
      | Some(z) => z |> Caret.set(Inner(0))
      }
    : z;

/* This creates three tokens; two from splitting the existing one,
 * and a new single-character token (or grout) in the middle. */
let split = (z: t, char: string, idx: int, t: Token.t): option(t) => {
  let (l, r) = Token.split_nth(idx, t);
  let id = Zipper.adjacent_monotile_or_new_id(Right, z);
  let+ z = z |> Caret.set(Outer) |> Zipper.delete(Right);
  let z =
    z
    |> new_shard(~id, ~d=Left, l)
    |> remold_regrout(Left)  /* Required for e.g. splitting ap(|) */
    |> new_shard(~id=Id.mk(), ~d=Right, r)
    |> expand_or_barf_neighbors;
  let z =
    Token.space == char && should_supress_space(z)
      ? z
      : z
        |> new_shard(~id=Id.mk(), ~d=Left, char)
        |> move_into_string_or_comment(char);
  remold_regrout(Right, z);
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

let expand_livelit = (z: t, ll: Language.LivelitCtx.raw_livelit) => {
  let seg =
    ExpToSegment.exp_to_segment(
      ~settings=
        ExpToSegment.Settings.of_core(~inline=true, Language.CoreSettings.on),
      ll.model_default,
    );
  let seg =
    switch (ll.model_default) {
    | {term: Tuple(_), _} => Segment.unparenthesize(seg)
    | _ => seg
    };
  let (l, name) = ListUtil.split_last(fst(z.relatives.siblings));
  let seg = [name, Piece.mk_tile(Form.get(ApExp), [seg])];
  let+ pr = ProjectorPerform.init(Livelit, seg);
  Zipper.update_siblings(((_, r)) => (l @ [pr], r), z);
};

let go =
    (~ctx: Language.Ctx.t=Language.Ctx.empty, char: string, z: t): option(t) => {
  /* If there's a selection, delete it before proceeding */
  let z = z.selection.content != [] ? Zipper.destroy_selection(z) : z;
  switch (z.caret, neighbor_shards(z)) {
  /* If we try to insert a quote inside an existing string, or a #
   * in a comment, we are instead moved to the righthand side of
   * the operand. Note that this behavior is load-bearing for the
   * current parsing approach including Paste */
  | (_, (_, Some(t))) when Token.closing_stringlit_or_comment(char, t) =>
    z |> Caret.set(Outer) |> Zipper.move(Right)
  | (Outer, (Some(t), _)) when Token.closing_stringlit_or_comment(char, t) =>
    Some(z)
  | (Outer, (Some(t), _)) when Token.is_livelit(t) && char == Token.space =>
    switch (Language.Ctx.lookup_livelit(ctx, Token.parse_livelit(t))) {
    | Some(ll) => expand_livelit(z, ll)
    | None => append_or_construct(char, z)
    }
  | (Inner(idx), (_, Some(t))) =>
    let idx = idx + 1;
    let new_token = Token.insert_nth(idx, char, t);
    let z = Caret.set(Inner(idx), z);
    Token.is_potential_token(new_token)
      ? z
        |> replace_shard(Right, new_token)
        |> Option.map(remold_regrout(Right))
      : split(z, char, idx, t);
  | (Inner(_), (_, None)) => None /* Impossible? */
  | (Outer, (_, Some(_))) =>
    let+ z_init = append_or_construct(char, z);
    let init_left_nhbr = Siblings.right_neighbor(z_init.relatives.siblings);
    let z =
      z_init
      |> remold_regrout(Left)  // problematique
      |> move_into_string_or_comment(char)
      |> Caret.set(
           switch (sibling_appendability(char, z_init)) {
           | Some((Right, _)) => Inner(0)
           | None
           | Some((Left, _)) => Outer
           },
         );
    //let init_left_nhbr = Siblings.right_neighbor(z.relatives.siblings);
    //let z = remold_regrout(d, z);
    let new_nhbr = Siblings.right_neighbor(z.relatives.siblings);
    switch (new_nhbr, z.caret, Zipper.move(Right, z)) {
    | (Some(p), Inner(_), Some(z))
        when Piece.is_grout(p) && new_nhbr != init_left_nhbr => z
    | _ => z
    };
  | (Outer, (_, None)) =>
    let+ z = append_or_construct(char, z);
    z |> remold_regrout(Left) |> move_into_string_or_comment(char);
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
