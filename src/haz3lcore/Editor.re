open Util;

module CachedSyntax = {
  type t('p) = {
    segment: Segment.t('p),
    measured: Measured.t,
    tiles: TileMap.t('p),
    /* The term-derived data structured below, may differ
     * from the term used for semantics. These terms are identical when
     * the backpack is empty. If the backpack is non-empty, then when we
     * make the term for semantics, we attempt to empty the backpack
     * according to some simple heuristics (~ try to empty it greedily
     * while moving rightwards from the current caret position).
     * this is currently necessary to have the cursorinfo/completion
     * workwhen the backpack is nonempty.
     *
     * This is a brittle part of the current implementation. there are
     * some other comments at some of the weakest joints; the biggest
     * issue is that dropping the backpack can add/remove grout, causing
     * certain ids to be present/non-present unexpectedly. */
    term_ranges: TermRanges.t('p),
    terms: TermMap.t,
    /* Since the introduction of shape_map below, caching projectors
     * here is almost vesigial (currently used only for error deco) */
    projectors: Id.Map.t(Base.projector('p)),
    /* The shape_map is used to leave space for projectors in the
     * underlying editor. In principle calculating this can involve
     * both static and dynamic information, so we cache this for perf */
    shape_map: ProjectorShape.Map.t,
  };

  // should not be serializing
  let sexp_of_t = _ => failwith("Editor.Meta.sexp_of_t");
  let t_of_sexp = _ => failwith("Editor.Meta.t_of_sexp");
  let yojson_of_t = _ => failwith("Editor.Meta.yojson_of_t");
  let t_of_yojson = _ => failwith("Editor.Meta.t_of_yojson");

  let mk_proj_shape_map =
      (
        type p,
        ~common,
        ~shape_of_projector,
        proj_map: Id.Map.t(Base.projector(p)),
      )
      : Id.Map.t(Util.ProjectorShape.t) =>
    Id.Map.map(shape_of_projector(~common), proj_map);

  let init =
      (
        type p,
        ~common,
        ~shape_of_projector,
        ~projector_to_term,
        ~sort: Sort.t,
        z: Zipper.t(p),
      )
      : t(p) => {
    let segment = Zipper.unselect_and_zip(z);
    let MakeTerm.{term: _, terms, projectors} =
      MakeTerm.go(sort, segment, ~of_projector=projector_to_term);
    let projector_shapes =
      mk_proj_shape_map(~common, ~shape_of_projector, projectors);
    {
      segment,
      term_ranges: TermRanges.mk(segment),
      tiles: TileMap.mk(segment),
      measured: Measured.of_segment(segment, projector_shapes),
      terms,
      projectors,
      shape_map: projector_shapes,
    };
  };
};

[@warning "-20"]
module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('p_k, 'p, 'p_a) = {
    // Constant
    id: Id.t,
    // Updated
    zipper: Calc.t(Zipper.t('p)),
    selection: Calc.t(unit), // separate flag to indicate only selection has changed
    col_target: option(int),
    // MakeTerm
    term: Calc.saved(Language.Any.t),
    sort: Calc.saved(Sort.t),
    // Calculated
    [@opaque]
    syntax: Calc.saved(CachedSyntax.t('p)),
    selection_ids: Calc.saved(list(Id.t)),
  };

  let mk = (type a, zipper: Zipper.t(a)) => {
    id: Id.mk(),
    zipper: NewValue(zipper),
    selection: NewValue(),
    col_target: None,
    term: Pending,
    sort: Pending,
    syntax: Calc.Pending,
    selection_ids: Calc.Pending,
  };

  let copy = ed => {
    {
      id: Id.mk(),
      zipper: ed.zipper,
      selection: ed.selection,
      col_target: ed.col_target,
      term: ed.term,
      sort: ed.sort,
      syntax: ed.syntax,
      selection_ids: ed.selection_ids,
    };
  };

  let get_z = model => model.zipper |> Calc.get_value;

  let indicated_term = (model: t('p_k, 'p, 'p_a)): option(Language.Any.t) => {
    let zipper = get_z(model);
    open OptUtil.Syntax;
    let* indicated_index = Indicated.index(zipper);
    switch (model.syntax) {
    | Pending =>
      print_endline("WARNING: get_indicated_term called on pending model");
      None;
    | Calculated(syntax) => Id.Map.find_opt(indicated_index, syntax.terms)
    };
  };

  type persistent = PersistentZipper.t;
  let persist = (f: 'p => 'q, model: t('p_k, 'p, 'p_a)) =>
    model |> get_z |> PersistentZipper.persist(f);
  let unpersist = (f, p) => p |> PersistentZipper.unpersist(f) |> mk;

  let sexp_of_t = (_, f, _, model: t('p_k, 'p, 'p_a)) =>
    model |> persist(f) |> PersistentZipper.sexp_of_t;
  let t_of_sexp = (_, f, _, s) =>
    s |> PersistentZipper.t_of_sexp |> unpersist(f);

  let to_move_s =
      (type p', model: t('p_k, p', 'p_a)): (module Move.S with type p = p') => {
    let syntax = Calc.get_saved_exc(model.syntax);
    module M: Move.S with type p = p' = {
      type p = p';
      let measured = syntax.measured;
      let term_ranges = syntax.term_ranges;
      let col_target = model.col_target |> Option.value(~default=0);
    };
    (module M);
  };

  let trailing_hole_ctx =
      (ed: t('p_k, 'p, 'p_a), info_map: Language.Statics.Map.t) => {
    let segment = Zipper.unselect_and_zip(ed |> get_z);
    let convex_grout = Segment.convex_grout(segment);
    // print_endline(String.concat("; ", List.map(Grout.show, convex_grout)));
    let last = Util.ListUtil.last_opt(convex_grout);
    switch (last) {
    | None => None
    | Some(grout) =>
      let id = grout.id;
      let info = Language.Statics.Map.lookup(id, info_map);
      switch (info) {
      | Some(info) => Some(Language.Info.ctx_of(info))
      | _ => None
      };
    };
  };

  let get_projector_model = (id: Id.t, m: t('p_k, 'p, 'p_a)): 'p => {
    //WARNING!! This is linear in the size of the program!
    let zipper = m |> get_z;
    let piece =
      Zipper.FindPiece.in_zipper(
        p =>
          switch (p) {
          | Projector(p) => p.id == id
          | _ => false
          },
        zipper,
      );
    switch (piece) {
    | Some(Projector(p)) => p.model
    | _ => failwith("Editor.Model.get_projector_model: no projector found")
    };
  };

  let get_cached_term = editor => Calc.get_saved_exc(editor.term);

  let get_web_id = (type p_k, type p, type p_a, model: t(p_k, p, p_a)) => {
    "editor_" ++ Id.str8(model.id);
  };

  let get_dimensions = (ed: t('p_k, 'p, 'p_a)) => {
    let measured = Calc.get_saved_exc(ed.syntax).measured;
    let segment = Calc.get_saved_exc(ed.syntax).segment;
    Point.{
      row: Measured.width(segment, measured),
      col: Measured.height(segment, measured),
    };
  };

  let split =
      (type p_k, type p, type p_a, ed: t(p_k, p, p_a), ids: list(Id.t))
      : Id.Map.t(t(p_k, p, p_a)) => {
    let segment = Zipper.unselect_and_zip(ed |> get_z);
    let seg_map = TermRanges.split(ids, segment);
    Id.Map.map(seg => seg |> Zipper.unzip |> mk, seg_map);
  };
};

module Update = {
  open Calc.Syntax;
  type t('p_k, 'p, 'p_a) = Action.t('p_k, 'p, 'p_a);

  let update =
      (
        type p,
        type p_k,
        type p_a,
        ~settings: Language.CoreSettings.t,
        ~projector_init,
        ~seg_of_projector,
        ~update_projector,
        ~livelit_projectors,
        a: Action.t(p_k, p, p_a),
        old_statics,
        model: Model.t(p_k, p, p_a),
      )
      : Action.Result.t(Model.t(p_k, p, p_a)) => {
    let seg_to_ed = seg => Zipper.unzip(seg) |> Model.mk |> Option.some;
    open Result.Syntax;
    let old_zipper = state.zipper;
    /* 1. Clear the autocomplete buffer if relevant. We clear the TyDi
     * (unparsed) buffer on every action except accept; for the LLM
     * (parsed) buffer, we accept resize actions to permit incremental
     * accepteance token-by-token or line-by-line */
    let clear_condition =
      (settings.assist && settings.statics && a != Buffer(Accept))
      && !(
           Selection.non_empty_parsed_buffer(state.zipper.selection)
           && (
             switch (a) {
             | Select(Resize(Local(_))) => true
             | _ => false
             }
           )
         );

    let state =
      clear_condition
        ? {
          ...state,
          zipper:
            Perform.go_z(
              ~settings,
              ~seg_to_ed,
              ~projector_init,
              ~seg_of_projector,
              ~update_projector,
              ~livelit_projectors,
              old_statics,
              Buffer(Clear),
              Model.to_move_s({
                state,
                syntax,
              }),
              state.zipper,
            )
            |> Action.Result.ok
            |> Option.value(~default=state.zipper),
        }
        : state;
    let syntax =
      if (clear_condition) {
        CachedSyntax.mark_old(syntax);
      } else {
        Calc.OldValue(zipper);
      };
    /* TODO(andrew): Apologize to matt for below.
       If a buffer clear happens above then we must recalculate the
       syntax cache as otherwise the measured, in particular caret_point,
       will be looking for tiles inside the buffer, for example if we try
       to click or move down to dismiss a completion.*/
    let syntax =
      if (clear_condition
          && Selection.non_empty_parsed_buffer(old_zipper.selection)) {
        CachedSyntax.calculate(
          state.zipper,
          old_statics.info_map,
          Id.Map.empty, //TODO
          syntax,
        );
      } else {
        syntax;
      };

    // 2. Record target column if moving up/down
    let col_target =
      switch (a) {
      | Move(Local(Up | Down))
      | Select(Resize(Local(Up | Down))) =>
        switch (model.col_target) {
        | Some(col) => Some(col)
        | None =>
          Some(
            Zipper.caret_point(
              Calc.get_saved_exc(model.syntax).measured,
              Calc.get_value(zipper),
            ).
              col,
          )
        }
      | _ => None
      };
    let model = {
      ...model,
      col_target,
    };

    // 3. Update the zipper
    let+ zipper =
      Perform.go_z(
        ~settings,
        ~projector_init,
        ~seg_of_projector,
        ~seg_to_ed,
        ~update_projector,
        ~livelit_projectors,
        old_statics,
        a,
        Model.to_move_s({
          state,
          syntax,
        }),
        state.zipper,
      );
    let zipper =
      Action.is_edit(a) || Calc.is_new(model.zipper)
        ? Calc.NewValue(zipper) : Calc.OldValue(zipper);
    let model = {
      ...model,
      zipper,
    };

    // 4. Mark selection as new
    let selection = Calc.NewValue();
    let model = {
      ...model,
      selection,
    };

    model;
  };

  let make_term =
      (type p, ~make_term_prj, ~sort, model: Model.t('p_k, p, 'p_a)) => {
    let new_sort = Calc.set(sort, model.sort);

    let updated_projectors: Hashtbl.t(Id.t, p) = Hashtbl.create(0);
    let of_projector = (~sort: Sort.t, ~id: Id.t, model: p) => {
      let (model', term) = make_term_prj(~sort, model);
      Hashtbl.add(updated_projectors, id, model');
      term |> Calc.get_value;
    };

    let term =
      model.term
      |> {
        let.calc z = model.zipper
        and.calc sort = new_sort;
        MakeTerm.go(
          ~of_projector,
          sort,
          Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true, z),
        ).
          term;
      };

    // Update the projectors in the zipper
    let zipper =
      ZipperBase.MapPiece.go(
        fun
        | Projector(p) =>
          switch (Hashtbl.find_opt(updated_projectors, p.id)) {
          | Some(model) => [
              Projector({
                ...p,
                model,
              }),
            ]
          | None => [Projector(p)]
          }
        | x => [x],
        model.zipper |> Calc.get_value,
      );
    let zipper =
      switch (model.zipper) {
      | NewValue(_) => Calc.NewValue(zipper)
      | OldValue(_) => Calc.OldValue(zipper)
      };

    let model = {
      ...model,
      zipper,
      term: term |> Calc.save,
      sort: new_sort |> Calc.save,
    };
    (model, term);
  };

  let calculate =
      (
        type p,
        ~common: Common.t,
        ~projector_init,
        ~projector_to_term,
        ~seg_of_projector,
        ~shape_of_projector,
        ~livelit_projectors,
        ~update_projector,
        ~calculate_projector,
        model: Model.t('p_k, p, 'p_a),
      ) => {
    let seg_to_ed = seg => Zipper.unzip(seg) |> Model.mk |> Option.some;

    // 1. Recalculate the autocomplete buffer if necessary
    let zipper =
      if (model.syntax != Calc.Pending) {
        let.calc_map zipper = model.zipper;
        if (common.settings.assist && common.settings.statics) {
          switch (
            Perform.go_z(
              ~settings=common.settings,
              ~seg_to_ed,
              ~projector_init,
              ~seg_of_projector,
              ~update_projector,
              ~livelit_projectors,
              common.statics,
              Buffer(Set(TyDi)),
              Model.to_move_s(model),
              zipper,
            )
          ) {
          | Ok(z) => z
          | Error(_) => zipper
          };
        } else {
          zipper;
        };
      } else {
        model.zipper;
      };
    let zipper =
      switch (model.zipper) {
      | NewValue(_) => Calc.NewValue(zipper |> Calc.get_value)
      | OldValue(_) => Calc.OldValue(zipper |> Calc.get_value)
      };
    let model = {
      ...model,
      zipper,
    };

    // 2. Recalculate Projector models
    let zipper =
      ZipperBase.MapPiece.go(
        fun
        | Projector(p) => [
            Projector({
              ...p,
              model: calculate_projector(~common, p.model),
            }),
          ]
        | x => [x],
        zipper |> Calc.get_value,
      );

    /* HACK: here we assume that the projector recalculations never
       require the syntax cache to be marked as old, unless the user
       made an edit action in this editor. We should eventually let
       projectors change their syntax cache based on static information */
    let zipper =
      switch (model.zipper) {
      | NewValue(_) => Calc.NewValue(zipper)
      | OldValue(_) => Calc.OldValue(zipper)
      };
    let model = {
      ...model,
      zipper,
    };

    // 3. Recalculate syntax cache
    let syntax =
      model.syntax
      |> {
        let.calc z = zipper;
        CachedSyntax.init(
          ~common,
          ~projector_to_term,
          ~shape_of_projector,
          ~sort=Calc.get_saved_exc(model.term) |> Language.Any.sort,
          z,
        );
      };
    let model = {
      ...model,
      syntax: syntax |> Calc.save,
    };

    // 4. Recalculate selection ids
    let selection_ids =
      model.selection_ids
      |> {
        let.calc () = model.selection
        and.calc z = zipper;
        Selection.selection_ids(z.selection);
      };
    let model = {
      ...model,
      selection_ids: selection_ids |> Calc.save,
    };

    // 5. Mark everything as old

    Model.{
      id: model.id,

      zipper: Calc.make_old(model.zipper),
      selection: Calc.make_old(model.selection),
      col_target: model.col_target,

      term: model.term,
      sort: model.sort,

      syntax: model.syntax,
      selection_ids: model.selection_ids,
    };
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t('p_k, 'p, 'p_a) = Model.t('p_k, 'p, 'p_a);
