open Zipper;
open Util;
open OptUtil.Syntax;

/* Syntax replacement operations to automatically run after insertion */

let exp_to_seg =
  ExpToSegment.exp_to_segment(
    ~settings=
      ExpToSegment.Settings.of_core(~inline=true, Language.CoreSettings.on),
  );

let invoked_projector = (name: string, syntax: Segment.t): option(Piece.t) => {
  let* name = Token.of_projector_invoke(name);
  let kind = ProjectorCore.Kind.of_name(name);
  ProjectorPerform.init(kind, syntax);
};

let expand_projector = (z: t): option(t) => {
  switch (z.relatives.siblings |> fst |> List.rev) {
  | [
      Tile({label: ["(", ")"], children: [syntax], _}),
      Tile({label: [name], _}),
      ...rest,
    ]
      when name == "^^probe" =>
    //TODO(andrew): clarify probe case
    Zipper.update_siblings(((_, r)) => (syntax @ List.rev(rest), r), z)
    |> MkRefractor.add_single(
         Segment.root_id(Segment.skel(syntax), syntax),
       )
    |> Option.some
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

let refractor_to_invoke =
    (kind: ProjectorCore.Kind.t, seg: Segment.t): Segment.t => [
  Piece.mk_tile(Form.mk_atom_op(Exp, Token.mk_projector_invoke(kind)), []),
  Piece.mk_tile(Form.get(ApExp), [seg]),
];

let projector_to_invoke = (pr: Base.projector): Segment.t =>
  refractor_to_invoke(pr.kind, Piece.unparenthesize(pr.syntax));

let expand_livelit = (~ctx, z: t): option(t) =>
  switch (z.relatives.siblings |> fst |> List.rev) {
  | [Secondary({content: Whitespace(w), _}), Tile({label: [t], _}), ..._]
      when Token.is_livelit(t) && w == Token.space =>
    let* ll = Language.Ctx.lookup_livelit(ctx, Token.parse_livelit(t));
    let seg = exp_to_seg(ll.model_default);
    let seg =
      switch (ll.model_default) {
      | {term: Tuple(_), _} => Segment.unparenthesize(seg)
      | _ => seg
      };
    let (l, _space) = ListUtil.split_last(fst(z.relatives.siblings));
    let (l, name) = ListUtil.split_last(l);
    let seg = [name, Piece.mk_tile(Form.get(ApExp), [seg])];
    let+ pr = ProjectorPerform.init(Livelit, seg);
    Zipper.update_siblings(((_, r)) => (l @ [pr], r), z);
  | _ => None
  };

/* This is a wrapper intended to effectuate after-insertion conditional
 * operations. This is done here as opposed to in Perform in order to
 * reflect operations we want performed by the parser, which uses
 * Insert.go as its primary driver. Triggers should be zipper to zipper
 * functions which conditionally perform a syntax operation which may
 * (or may not) take caret position into account. Morally these should
 * be mutually exclusive; otherwise the order below is load-bearing. */
let insert = (~ci: option(Language.Info.t), z: t): t => {
  let ctx =
    ci
    |> Option.map(Language.Info.ctx_of)
    |> Option.value(~default=Language.Ctx.empty);
  let triggers = [expand_projector, expand_livelit(~ctx)];
  List.fold_left((z, f) => Option.value(f(z), ~default=z), z, triggers);
};

/* These are just alternate conditional deletion logic. */
let destruct = (z: t): option(t) =>
  switch (z.relatives.siblings |> fst |> ListUtil.last_opt) {
  | Some(Projector({syntax, kind, _})) =>
    let (l, _) = ListUtil.split_last(fst(z.relatives.siblings));
    let last =
      switch (kind, syntax) {
      | (Livelit, Tile({children: [[name, ..._]], _})) => [name]
      | _ => Piece.unparenthesize(syntax)
      };
    Some(Zipper.update_siblings(((_, r)) => (l @ last, r), z));
  | _ => None
  };

let refractor_seg_to_seg =
    (refractors: Id.Map.t(Base.projector), seg: Segment.t)
    : (Id.Map.t(Base.projector), Segment.t) => {
  /* This function transforms a segment by wrapping terms that have refractors
   * with their invocation syntax (e.g., ^^probe(...)).
   *
   * Key insight: We recursively process children first, then concatenate:
   *   left_result @ middle_slice @ right_result
   * where middle_slice comes from the ORIGINAL segment (preserving Secondary).
   * This way original indices stay valid since we only read, never modify. */

  let rec go =
          (map: Id.Map.t(Base.projector), skel: Skel.t)
          : (Id.Map.t(Base.projector), Segment.t) => {
    let (map, result) =
      switch (skel) {
      | Op(root) =>
        /* Leaf node: slice the full range including any Secondary.
         * For n-ary ops like tuples, root is an Aba with multiple indices. */
        let indices = Aba.get_as(root);
        let first_idx = List.hd(indices);
        let last_idx = ListUtil.last(indices);
        (map, ListUtil.sublist((first_idx, last_idx + 1), seg));

      | Pre(root, child) =>
        /* Prefix operator: root pieces come before child */
        let (_, child_start) = Skel.range(child);
        let root_indices = Aba.get_as(root);
        let root_start = List.hd(root_indices);

        let (map, child_result) = go(map, child);

        /* Prefix slice: from root start to just before child */
        let prefix = ListUtil.sublist((root_start, child_start), seg);
        (map, prefix @ child_result);

      | Post(child, root) =>
        /* Postfix operator: child comes before root pieces */
        let (_, child_end) = Skel.range(child);
        let root_indices = Aba.get_as(root);
        let root_end = ListUtil.last(root_indices);

        let (map, child_result) = go(map, child);

        /* Postfix slice: from after child to end of root */
        let postfix = ListUtil.sublist((child_end + 1, root_end + 1), seg);
        (map, child_result @ postfix);

      | Bin(left, _root, right) =>
        /* Binary operator: left @ middle @ right
         * Middle includes the operator and surrounding Secondary */
        let (_, left_end) = Skel.range(left);
        let (right_start, _) = Skel.range(right);

        let (map, left_result) = go(map, left);
        let (map, right_result) = go(map, right);

        /* Middle slice from ORIGINAL segment - this preserves Secondary! */
        let middle = ListUtil.sublist((left_end + 1, right_start), seg);
        (map, left_result @ middle @ right_result);
      };

    /* Check if this term needs to be wrapped with a refractor invocation */
    let root_id = Segment.root_id(skel, seg);
    switch (Id.Map.find_opt(root_id, map)) {
    | Some(pr) => (
        Id.Map.remove(root_id, map),
        refractor_to_invoke(pr.kind, result),
      )
    | None => (map, result)
    };
  };

  if (Id.Map.is_empty(refractors)) {
    (refractors, seg);
  } else {
    let (map, new_seg) = go(refractors, Segment.skel(seg));
    (map, new_seg);
  };
};
