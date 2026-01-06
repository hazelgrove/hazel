open Zipper;
open Util;
open OptUtil.Syntax;

/* Syntax replacement operations to automatically run after insertion */

let exp_to_seg =
  ExpToSegment.exp_to_segment(
    ~settings=
      ExpToSegment.Settings.of_core(~inline=true, Language.CoreSettings.on),
  );

let invoked_projector =
    (
      ~projector_init: (ProjectorKind.t, Segment.t) => option(Piece.t),
      name: string,
      syntax: Segment.t,
    )
    : option(Piece.t) => {
  let* name = Token.of_projector_invoke(name);
  let kind = ProjectorKind.of_name(name);
  projector_init(kind, syntax);
};

let expand_projector =
    (~projector_init: (ProjectorKind.t, Segment.t) => option(Piece.t), z: t)
    : option(t) => {
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
    let+ piece = invoked_projector(~projector_init, name, syntax);
    Zipper.update_siblings(
      ((_, r)) => ([piece, ...rest] |> List.rev, r),
      z,
    );
  /* Special case for reparsing of projectors placed on holes */
  | [Tile({label: ["()"], _}), Tile({label: [name], _}), ...rest]
      when Token.is_projector_invoke(name) =>
    let+ piece =
      invoked_projector(~projector_init, name, [Piece.mk_grout(Convex)]);
    Zipper.update_siblings(
      ((_, r)) => ([piece, ...rest] |> List.rev, r),
      z,
    );
  | _ => None
  };
};

/* Convert a projector to its textual invocation form: @name(syntax)
 * This requires knowing how to extract the kind and segment from the model.
 * See Perform.projector_to_invoke for a parameterized version. */
let projector_to_invoke =
    (
      ~get_kind: Projector.model => ProjectorKind.t,
      ~seg_of_projector: Projector.model => Segment.t,
      pr: Base.projector,
    )
    : Segment.t => [
  Piece.mk_tile(
    Form.mk_atom_op(Exp, Token.mk_projector_invoke(get_kind(pr.model))),
    [],
  ),
  Piece.mk_tile(Form.get(ApExp), [seg_of_projector(pr.model)]),
];

let expand_livelit =
    (
      ~projector_init: (ProjectorKind.t, Segment.t) => option(Piece.t),
      ~ctx,
      z: t,
    )
    : option(t) =>
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
    let+ pr = projector_init(Livelit, seg);
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
let insert =
    (
      ~projector_init: (ProjectorKind.t, Segment.t) => option(Piece.t),
      ~ctx: Language.Ctx.t=Language.Ctx.empty,
      z: t,
    )
    : t => {
  let triggers = [
    expand_projector(~projector_init),
    expand_livelit(~projector_init, ~ctx),
  ];
  List.fold_left((z, f) => Option.value(f(z), ~default=z), z, triggers);
};

/* These are just alternate conditional deletion logic.
 * When deleting a projector, replace it with its underlying syntax.
 *
 * NOTE: This approach reconstitutes the segment from the term via
 * ExpToSegment, which does NOT preserve the original formatting/whitespace
 * of the projector's underlying syntax. This is acceptable for now but
 * may need revisiting if formatting preservation becomes important. */
let destruct = (z: t): option(t) =>
  switch (z.relatives.siblings |> fst |> ListUtil.last_opt) {
  | Some(Projector(pr)) =>
    let (l, _) = ListUtil.split_last(fst(z.relatives.siblings));
    let kind = Projector.kind_of_model(pr.model);
    let term = Projector.term_of_model(pr.model);
    let syntax =
      ExpToSegment.any_to_segment(
        ~settings=
          ExpToSegment.Settings.of_core(
            ~inline=true,
            Language.CoreSettings.on,
          ),
        term,
      );
    let last =
      switch (kind, syntax) {
      /* Livelit special case: expand to just the livelit name token.
       * TODO: Review if this behavior is still desired/correct. */
      | (Livelit, [Tile({children: [[name, ..._]], _})]) => [name]
      | _ => Segment.unparenthesize(syntax)
      };
    Some(Zipper.update_siblings(((_, r)) => (l @ last, r), z));
  | _ => None
  };
