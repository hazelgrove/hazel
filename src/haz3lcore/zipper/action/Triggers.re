open Zipper;
open Util;
open OptUtil.Syntax;

/* Syntax replacement operations to automatically run after insertion */

/* Check if a string is a refractor trigger name (e.g., "^^type", "^^probe") */
let is_refractor_trigger = (s: string): bool =>
  String.length(s) > 2
  && String.sub(s, 0, 2) == "^^"
  && {
    let kind_name = String.sub(s, 2, String.length(s) - 2);
    ProjectorCore.Kind.is_name(kind_name)
    && ProjectorCore.Kind.is_refractor(ProjectorCore.Kind.of_name(kind_name));
  };

/* Parse a refractor trigger name to get the kind */
let of_refractor_trigger = (s: string): ProjectorCore.Kind.t =>
  ProjectorCore.Kind.of_name(String.sub(s, 2, String.length(s) - 2));

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
      when is_refractor_trigger(name) =>
    /* Left siblings are stored as [oldest, ..., newest]. After List.rev we have
     * [newest(parens), ^^refractor, ...rest] where rest is [third_newest, ..., oldest].
     * We want syntax in the newest position: [oldest, ..., third_newest, syntax...] */
    let kind = of_refractor_trigger(name);
    Zipper.update_siblings(((_, r)) => (List.rev(rest) @ syntax, r), z)
    |> Zipper.add_manual(
         Segment.root_id(Segment.skel(syntax), syntax),
         kind,
       )
    |> Option.some;

  | [
      Tile({label: ["(", ")"], children: [syntax], _}),
      Tile({label: [name], _}),
      ...rest,
    ]
      when Token.is_projector_invoke(name) =>
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
    (refractors: Zipper.Refractor.Map.t, seg: Segment.t)
    : (Zipper.Refractor.Map.t, Segment.t) => {
  /* This function transforms a segment by wrapping terms that have refractors
   * with their invocation syntax (e.g., ^^probe(...)).
   *
   * Key insight: We recursively process ALL child skeletons (including those
   * inside compound operator Abas), interleaving their results with slices
   * from the original segment to preserve Secondary (whitespace/comments).
   *
   * The Aba structure for a compound operator like `let x = 1 in`:
   *   ([let_idx, eq_idx, in_idx], [pat_skel, def_skel])
   * The children between delimiters must be recursively processed. */

  /* Process an Aba root, returning segment from first_a to last_a (inclusive).
   * Recursively processes all child skeletons in the Aba. */
  let rec go_aba =
          (map: Zipper.Refractor.Map.t, root: Skel.root)
          : (Zipper.Refractor.Map.t, Segment.t) => {
    let indices = Aba.get_as(root);
    let children = Aba.get_bs(root);
    switch (indices, children) {
    | ([single_idx], []) =>
      /* Atomic operator: just slice around this single index */
      (map, ListUtil.sublist((single_idx, single_idx + 1), seg))
    | ([first_idx, ...rest_indices], children) =>
      /* Compound operator: interleave index slices with processed children.
       * For indices [i0, i1, i2] and children [c0, c1]:
       *   slice(i0, c0_start) @ go(c0) @ slice(c0_end+1, i1) @
       *   slice(i1, c1_start) @ go(c1) @ slice(c1_end+1, i2) @ slice(i2, i2+1) */
      let rec go_interleave =
              (
                map: Zipper.Refractor.Map.t,
                prev_idx: int,
                indices: list(int),
                children: list(Skel.t),
              )
              : (Zipper.Refractor.Map.t, Segment.t) =>
        switch (indices, children) {
        | ([], []) =>
          /* After last index: include slice for the final token */
          (map, ListUtil.sublist((prev_idx, prev_idx + 1), seg))
        | ([next_idx, ...rest_indices], [child, ...rest_children]) =>
          /* Process: slice from prev token to child, then child, then continue */
          let (child_start, child_end) = Skel.range(child);
          let before_child = ListUtil.sublist((prev_idx, child_start), seg);
          let (map, child_result) = go(map, child);
          let after_child = ListUtil.sublist((child_end + 1, next_idx), seg);
          let (map, rest_result) =
            go_interleave(map, next_idx, rest_indices, rest_children);
          (map, before_child @ child_result @ after_child @ rest_result);
        | _ => failwith("Aba invariant violated: indices/children mismatch")
        };
      go_interleave(map, first_idx, rest_indices, children);
    | ([], _) => failwith("Aba invariant violated: empty indices")
    };
  }
  and go =
      (map: Zipper.Refractor.Map.t, skel: Skel.t)
      : (Zipper.Refractor.Map.t, Segment.t) => {
    let (map, result) =
      switch (skel) {
      | Op(root) =>
        /* Operator (may be compound like tuple): process the Aba */
        go_aba(map, root)

      | Pre(root, child) =>
        /* Prefix operator: root Aba comes before the trailing child */
        let root_indices = Aba.get_as(root);
        let root_end = ListUtil.last(root_indices);
        let (child_start, _) = Skel.range(child);

        let (map, root_result) = go_aba(map, root);
        let between = ListUtil.sublist((root_end + 1, child_start), seg);
        let (map, child_result) = go(map, child);

        (map, root_result @ between @ child_result);

      | Post(child, root) =>
        /* Postfix operator: child comes before root Aba */
        let (_, child_end) = Skel.range(child);
        let root_indices = Aba.get_as(root);
        let root_start = List.hd(root_indices);

        let (map, child_result) = go(map, child);
        let between = ListUtil.sublist((child_end + 1, root_start), seg);
        let (map, root_result) = go_aba(map, root);

        (map, child_result @ between @ root_result);

      | Bin(left, root, right) =>
        /* Binary operator: left @ (root Aba) @ right */
        let (_, left_end) = Skel.range(left);
        let (right_start, _) = Skel.range(right);
        let root_indices = Aba.get_as(root);
        let root_start = List.hd(root_indices);
        let root_end = ListUtil.last(root_indices);

        let (map, left_result) = go(map, left);
        let before_root = ListUtil.sublist((left_end + 1, root_start), seg);
        let (map, root_result) = go_aba(map, root);
        let after_root = ListUtil.sublist((root_end + 1, right_start), seg);
        let (map, right_result) = go(map, right);

        (
          map,
          left_result @ before_root @ root_result @ after_root @ right_result,
        );
      };

    /* Check if this term needs to be wrapped with a refractor invocation */
    let root_id = Segment.root_id(skel, seg);
    switch (Id.Map.find_opt(root_id, map)) {
    | Some(entry) => (
        Id.Map.remove(root_id, map),
        refractor_to_invoke(entry.kind, result),
      )
    | None => (map, result)
    };
  };

  if (Id.Map.is_empty(refractors)) {
    (refractors, seg);
  } else {
    /* Segment.skel throws exceptions for incomplete/malformed segments
     * (e.g., "1 +" without the right operand). In such cases, return
     * the segment unchanged. */
    try({
      let skel = Segment.skel(seg);
      let (skel_start, skel_end) = Skel.range(skel);
      let (map, new_seg) = go(refractors, skel);
      /* Preserve any leading/trailing Secondary (whitespace/comments) that
       * fall outside the skel range, since skel only tracks tile indices */
      let leading = ListUtil.sublist((0, skel_start), seg);
      let trailing =
        ListUtil.sublist((skel_end + 1, List.length(seg)), seg);
      (map, leading @ new_seg @ trailing);
    }) {
    | Skel.Nonconvex_segment
    | Failure(_) => (refractors, seg)
    };
  };
};
