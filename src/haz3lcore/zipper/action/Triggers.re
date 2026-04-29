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
    let z =
      Zipper.update_siblings(((_, r)) => (List.rev(rest) @ syntax, r), z);
    let skel = Segment.skel(syntax);
    switch (Segment.root_id(skel, syntax)) {
    | Some(root_id) => Zipper.add_manual(root_id, kind, z) |> Option.some
    | None => Some(z)
    };

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
    let+ piece = invoked_projector(name, []);
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

let projector_to_invoke_text = (pr: Base.projector): Segment.t =>
  refractor_to_invoke_text(pr.kind, Piece.unparenthesize(pr.syntax));

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

/* Parameterized version: takes a wrapper function for customizing output */
let refractor_seg_to_seg_with =
    (
      ~wrapper: (ProjectorCore.Kind.t, Segment.t) => Segment.t,
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

  /* Helper: get first/last real piece index from a root's piece_refs */
  let first_piece_idx = (root: Skel.root): option(int) =>
    List.find_map(
      fun
      | Skel.Piece(idx) => Some(idx)
      | Skel.Hole(_) => None,
      Aba.get_as(root),
    );
  let last_piece_idx = (root: Skel.root): option(int) =>
    List.find_map(
      fun
      | Skel.Piece(idx) => Some(idx)
      | Skel.Hole(_) => None,
      List.rev(Aba.get_as(root)),
    );

  /* Process an Aba root, returning the segment spanning its real pieces.
   * Recursively processes all child skeletons in the Aba.
   * Hole refs are skipped (they have no physical extent in the segment). */
  let rec go_aba =
          (map: Zipper.Refractor.RefractorList.t, root: Skel.root)
          : (Zipper.Refractor.RefractorList.t, Segment.t) => {
    let refs = Aba.get_as(root);
    let children = Aba.get_bs(root);
    /* Walk through refs and children linearly, tracking the last
     * real piece index for slicing boundaries. Holes are skipped. */
    let rec walk =
            (
              map: Zipper.Refractor.RefractorList.t,
              last_idx: option(int),
              refs: list(Skel.piece_ref),
              children: list(Skel.t),
            )
            : (Zipper.Refractor.RefractorList.t, Segment.t) =>
      switch (refs) {
      | [] => (map, [])
      | [ref] =>
        /* Last ref in the Aba: include gap secondary between last
         * tracked position and this piece */
        switch (ref) {
        | Skel.Piece(idx) =>
          let gap =
            switch (last_idx) {
            | Some(prev) when prev + 1 < idx =>
              ListUtil.sublist((prev + 1, idx), seg)
            | _ => []
            };
          (map, gap @ ListUtil.sublist((idx, idx + 1), seg));
        | Skel.Hole(_) => (map, [])
        }
      | [ref, ...rest_refs] =>
        switch (children) {
        | [] => failwith("Aba invariant: more refs than children + 1")
        | [child, ...rest_children] =>
          let (map, ref_seg, next_last_idx) =
            switch (ref) {
            | Skel.Piece(idx) => (
                map,
                ListUtil.sublist((idx, idx + 1), seg),
                Some(idx),
              )
            | Skel.Hole(_) => (map, [], last_idx)
            };
          /* Process the child skel between this ref and the next */
          let (map, child_result) = go(map, child);
          /* Slice the gap between this ref (or last real piece) and child,
             and between child and next ref */
          let child_range = Skel.range(child);
          let before_child =
            switch (next_last_idx, child_range) {
            | (Some(prev), Some((cs, _))) =>
              ListUtil.sublist((prev + 1, cs), seg)
            | _ => []
            };
          let (map, rest_result) =
            switch (child_range) {
            | Some((_, ce)) => walk(map, Some(ce), rest_refs, rest_children)
            | None => walk(map, next_last_idx, rest_refs, rest_children)
            };
          (map, ref_seg @ before_child @ child_result @ rest_result);
        }
      };
    walk(map, None, refs, children);
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
        let root_end = last_piece_idx(root);
        let child_range = Skel.range(child);

        let (map, root_result) = go_aba(map, root);
        let between =
          switch (root_end, child_range) {
          | (Some(re), Some((cs, _))) =>
            ListUtil.sublist((re + 1, cs), seg)
          | _ => []
          };
        let (map, child_result) = go(map, child);

        (map, root_result @ between @ child_result);

      | Post(child, root) =>
        /* Postfix operator: child comes before root Aba */
        let child_range = Skel.range(child);
        let root_start = first_piece_idx(root);

        let (map, child_result) = go(map, child);
        let between =
          switch (child_range, root_start) {
          | (Some((_, ce)), Some(rs)) =>
            ListUtil.sublist((ce + 1, rs), seg)
          | _ => []
          };
        let (map, root_result) = go_aba(map, root);

        (map, child_result @ between @ root_result);

      | Bin(left, root, right) =>
        /* Binary operator: left @ (root Aba) @ right */
        let left_range = Skel.range(left);
        let right_range = Skel.range(right);
        let root_start = first_piece_idx(root);
        let root_end = last_piece_idx(root);

        let (map, left_result) = go(map, left);
        let before_root =
          switch (left_range, root_start) {
          | (Some((_, le)), Some(rs)) =>
            ListUtil.sublist((le + 1, rs), seg)
          | (Some((_, le)), None) =>
            /* Root is all holes: include everything up to right */
            switch (right_range) {
            | Some((rs, _)) => ListUtil.sublist((le + 1, rs), seg)
            | None => []
            }
          | _ => []
          };
        let (map, root_result) = go_aba(map, root);
        let after_root =
          switch (root_end, right_range) {
          | (Some(re), Some((rs, _))) =>
            ListUtil.sublist((re + 1, rs), seg)
          | (None, _) => [] /* Already handled in before_root */
          | _ => []
          };
        let (map, right_result) = go(map, right);

        (
          map,
          left_result @ before_root @ root_result @ after_root @ right_result,
        );
      };

    /* Check if this term needs to be wrapped with a refractor invocation */
    switch (Segment.root_id(skel, seg)) {
    | Some(root_id) =>
      switch (List.assoc_opt(root_id, map)) {
      | Some(entry) => (
          ListUtil.remove_assoc(root_id, map),
          wrapper(entry.kind, result),
        )
      | None => (map, result)
      }
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
      switch (Skel.range(skel)) {
      | None => (refractors, seg)
      | Some((skel_start, skel_end)) =>
        let (map, new_seg) = go(refractors, skel);
        /* Preserve any leading/trailing Secondary (whitespace/comments) that
         * fall outside the skel range, since skel only tracks tile indices */
        let leading = ListUtil.sublist((0, skel_start), seg);
        let trailing =
          ListUtil.sublist((skel_end + 1, List.length(seg)), seg);
        (map, leading @ new_seg @ trailing);
      };
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
  refractor_seg_to_seg_with(~wrapper=refractor_to_invoke, refractors, seg);

/* Text-only version using Unicode brackets for CLI output */
let refractor_seg_to_seg_text =
    (refractors: Refractor.RefractorList.t, seg: Segment.t)
    : (Refractor.RefractorList.t, Segment.t) =>
  refractor_seg_to_seg_with(
    ~wrapper=refractor_to_invoke_text,
    refractors,
    seg,
  );
