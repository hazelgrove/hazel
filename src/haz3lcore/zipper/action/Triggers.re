open Zipper;
open Util_web;
open OptUtil.Syntax;

/* Syntax replacement operations to automatically run after insertion */

/* A trigger is written `^^kind` or `^^kind_opt`: the base name picks the
   projector kind, the option after `_` a non-default model (`^^probe_table`
   = a probe with the table renderer active). Token owns the `^^` prefix and
   the `_` split, so this takes a BARE kind name. Refractors are the
   additive-decoration kinds (probe, statics); other kinds answer None.
     "probe"                ==> Some(Probe)
     "slider"               ==> None  (a projector kind, not a refractor)
     "probe_table" / "nope" ==> None  (not a kind name) */
let refractor_kind_of_name = (name: string): option(ProjectorCore.Kind.t) =>
  ProjectorCore.Kind.of_name_opt(name)
  |> OptUtil.filter(ProjectorCore.Kind.is_refractor);

/* Same, for a whole trigger token; None when it is not a trigger at all. */
let refractor_kind_of_token = (s: string): option(ProjectorCore.Kind.t) =>
  Option.bind(Token.of_projector_invoke_base(s), refractor_kind_of_name);

/* Is this whole token a refractor trigger (e.g. "^^statics", "^^probe")?
   "^^probe" / "^^probe_table" ==> true
   "^^slider" / "let" / "^^"   ==> false */
let is_refractor_trigger = (s: string): bool =>
  Option.is_some(refractor_kind_of_token(s));

/* Parse a refractor trigger name to get the kind. Partial: only valid on
   strings is_refractor_trigger accepts.
     "^^probe" / "^^probe_table" ==> Probe */
let of_refractor_trigger = (s: string): ProjectorCore.Kind.t =>
  Option.get(refractor_kind_of_token(s));

let refractor_model_of_opt =
    (kind: ProjectorCore.Kind.t, opt: string): option(string) =>
  switch (kind) {
  | Probe => ProbeProj.model_string_for_renderer(opt)
  | _ => None
  };

let refractor_opt_of_model =
    (kind: ProjectorCore.Kind.t, model: string): option(string) =>
  switch (kind) {
  | Probe => ProbeProj.renderer_of_model_string(model)
  | _ => None
  };

/* Full-token parse: kind plus the model its option selects (if any).
   Used by trigger expansion and by text-slide loading. The model is the
   serialized projector model, not the option name that picked it.
     "^^probe"       ==> Some((Probe, None))
     "^^probe_table" ==> Some((Probe, Some("((active_renderer(...)))"))) */
let refractor_of_invoke_token =
    (token: string): option((ProjectorCore.Kind.t, option(string))) => {
  /* one strip-and-split for both halves, rather than re-parsing the
     token once for the kind and again for the option */
  let* body = Token.of_projector_invoke(token);
  let (name, opt) = Token.split_invoke_opt(body);
  let+ kind = refractor_kind_of_name(name);
  (kind, Option.bind(opt, refractor_model_of_opt(kind)));
};

let exp_to_seg =
  ExpToSegment.exp_to_segment(
    ~settings=
      ExpToSegment.Settings.of_core(~inline=true, Language.CoreSettings.on),
  );

let invoked_projector = (name: string, syntax: Segment.t): option(Piece.t) => {
  let* name = Token.of_projector_invoke(name);
  let kind = ProjectorCore.Kind.of_name(name);
  /* Statics haven't run yet at trigger time, so we pass the empty
   * elaborated expression. This means elaborate_syntax projectors
   * will fall back to the raw syntax path, which is correct —
   * elaboration happens on the next statics cycle after init. */
  ProjectorPerform.init(
    kind,
    syntax,
    ~elaborated=CachedStatics.empty.elaborated,
  );
};

/* Re-pin the refractor triggers a ~collect_refractors parse consumed from
   the source. FastParse reports (id, verbatim token) but stays below the
   action layer, so the token is parsed back into a kind here. Shared by
   every fast-first parse: text-slide loading and paste. */
let apply_refractors = (refractors: list((Id.t, string)), z: t): t =>
  List.fold_left(
    (z, (id, trigger)) =>
      switch (refractor_of_invoke_token(trigger)) {
      | Some((kind, model)) => ZipperBase.add_manual(~model?, id, kind, z)
      | None => z
      },
    z,
    refractors,
  );

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
    let (kind, model) =
      switch (refractor_of_invoke_token(name)) {
      | Some(km) => km
      | None => (of_refractor_trigger(name), None)
      };
    Zipper.update_siblings(((_, r)) => (List.rev(rest) @ syntax, r), z)
    |> Zipper.add_manual(
         ~model?,
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
    (~model: option(string)=?, kind: ProjectorCore.Kind.t, seg: Segment.t)
    : Segment.t => {
  let opt_suffix =
    switch (Option.bind(model, refractor_opt_of_model(kind))) {
    | Some(opt) => "_" ++ opt
    | None => ""
    };
  [
    Piece.mk_tile(
      Form.mk_atom_op(Exp, Token.mk_projector_invoke(kind) ++ opt_suffix),
      [],
    ),
    Piece.mk_tile(Form.get(ApExp), [seg]),
  ];
};

/* Text-only version using Unicode brackets for CLI output.
 * Only wraps probes, not other projector kinds. */
let refractor_to_invoke_text =
    (kind: ProjectorCore.Kind.t, seg: Segment.t): Segment.t =>
  switch (kind) {
  | Probe =>
    [Piece.mk_tile(Form.mk_atom_op(Exp, Token.probe_start), []), ...seg]
    @ [Piece.mk_tile(Form.mk_atom_op(Exp, Token.probe_end), [])]
  | _ => refractor_to_invoke(kind, seg)
  };

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
    /* No statics available at trigger time; empty elaborated is fine
     * since Livelit has elaborate_syntax=false. */
    let+ pr =
      ProjectorPerform.init(
        Livelit,
        seg,
        ~elaborated=CachedStatics.empty.elaborated,
      );
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

/* Parameterized version: takes a wrapper function for customizing output */
let refractor_seg_to_seg_with =
    (
      ~wrapper: (Refractor.entry, Segment.t) => Segment.t,
      refractors: Zipper.Refractor.RefractorList.t,
      seg: Segment.t,
    )
    : (Zipper.Refractor.RefractorList.t, Segment.t) => {
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
          (map: Zipper.Refractor.RefractorList.t, root: Skel.root)
          : (Zipper.Refractor.RefractorList.t, Segment.t) => {
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
                map: Zipper.Refractor.RefractorList.t,
                prev_idx: int,
                indices: list(int),
                children: list(Skel.t),
              )
              : (Zipper.Refractor.RefractorList.t, Segment.t) =>
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
      (map: Zipper.Refractor.RefractorList.t, skel: Skel.t)
      : (Zipper.Refractor.RefractorList.t, Segment.t) => {
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
    switch (List.assoc_opt(root_id, map)) {
    | Some(entry) => (
        ListUtil.remove_assoc(root_id, map),
        wrapper(entry, result),
      )
    | None => (map, result)
    };
  };

  if (List.is_empty(refractors)) {
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

/* Standard version using ^^probe(...) syntax */
let refractor_seg_to_seg =
    (refractors: Refractor.RefractorList.t, seg: Segment.t)
    : (Refractor.RefractorList.t, Segment.t) =>
  refractor_seg_to_seg_with(
    ~wrapper=
      (entry: Refractor.entry, seg) =>
        refractor_to_invoke(~model=entry.model, entry.kind, seg),
    refractors,
    seg,
  );

/* Text-only version using Unicode brackets for CLI output */
let refractor_seg_to_seg_text =
    (refractors: Refractor.RefractorList.t, seg: Segment.t)
    : (Refractor.RefractorList.t, Segment.t) =>
  refractor_seg_to_seg_with(
    ~wrapper=
      (entry: Refractor.entry, seg) =>
        refractor_to_invoke_text(entry.kind, seg),
    refractors,
    seg,
  );
