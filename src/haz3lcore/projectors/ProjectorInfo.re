/* Gather utility functions/values to be passed to the projector.
 * See ProjectorBase.utility definition for more information */
let utility: ProjectorBase.utility = {
  let seg_to_term = MakeTerm.any;
  let term_to_seg =
    ExpToSegment.any_to_pretty(
      ~settings={
        ...ExpToSegment.Settings.of_core(~inline=false, CoreSettings.off),
        show_unknown_as_hole: false,
      },
    );
  let term_to_syntax = (any: Any.t): Base.piece =>
    switch (term_to_seg(any)) {
    | [e] => e
    | seg =>
      let sort = Segment.sort_of(Segment.skel(seg), seg);
      switch (sort) {
      | Exp => Piece.mk_tile(Form.get("parens_exp"), [seg])
      | Pat => Piece.mk_tile(Form.get("parens_pat"), [seg])
      | Typ => Piece.mk_tile(Form.get("parens_typ"), [seg])
      | TPat
      | Rul
      | Any
      | Nul => failwith("Projector: term_to_syntax")
      };
    };
  let lift_syntax = (fn: Any.t => Any.t, piece: Base.piece): Base.piece =>
    [piece] |> seg_to_term |> fn |> term_to_syntax;
  {term_to_seg, seg_to_term, lift_syntax};
};

let mk_info =
    (
      id: Id.t,
      p: Piece.projector,
      ~cached_statics: CachedStatics.t,
      ~dynamics: Dynamics.Map.t,
    )
    : ProjectorBase.info => {
  id,
  syntax: p.syntax,
  statics: Statics.Map.lookup(id, cached_statics.info_map),
  dynamics: Dynamics.Map.lookup(id, dynamics),
  utility,
};

module Shape = {
  let of_info =
      (p: Base.projector, info: ProjectorBase.info): ProjectorCore.shape => {
    let (module P) = ProjectorInit.to_module(p.kind);
    P.placeholder(p.model, info);
  };

  let of_map =
      (statics: Statics.Map.t, dynamics: Dynamics.Map.t, p: Base.projector)
      : ProjectorCore.shape => {
    let statics = Statics.Map.lookup(p.id, statics);
    let dynamics = Dynamics.Map.lookup(p.id, dynamics);
    of_info(p, {id: p.id, syntax: p.syntax, statics, dynamics, utility});
  };

  let of_map_default = of_map(Id.Map.empty, Id.Map.empty);

  let token = (shape: ProjectorCore.shape): string =>
    switch (shape.vertical) {
    | Inline
    | Tab(_) => String.make(shape.horizontal, ' ')
    | Block(num_lb) =>
      String.make(num_lb, '\n') ++ String.make(shape.horizontal, ' ')
    };
};
