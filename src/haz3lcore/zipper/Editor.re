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

  let init =
      (
        type p,
        ~shape_of_projector,
        ~projector_to_term,
        ~info_map,
        ~dyn_map,
        ~sort: Sort.t,
        z: Zipper.t(p),
      )
      : t(p) => {
    let segment = Zipper.unselect_and_zip(z);
    let MakeTerm.{term: _, terms, projectors} =
      MakeTerm.go(sort, segment, ~of_projector=projector_to_term);
    let projector_shapes =
      ProjectorInfo.ShapeMapSemantics.mk(
        ~shape_of_projector,
        projectors,
        info_map,
        dyn_map,
      );
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
    // Updated
    zipper: Calc.t(Zipper.t('p)),
    selection: Calc.t(unit), // separate flag to indicate only selection has changed
    col_target: option(int),
    // MakeTerm
    term: Calc.saved(Any.t),
    sort: Calc.saved(Calc.t(Sort.t)),
    // Calculated
    [@opaque]
    syntax: Calc.saved(CachedSyntax.t('p)),
    selection_ids: Calc.saved(list(Id.t)),
  };

  let mk = (type a, zipper: Zipper.t(a)) => {
    zipper: NewValue(zipper),
    selection: NewValue(),
    col_target: None,
    term: Pending,
    sort: Pending,
    syntax: Calc.Pending,
    selection_ids: Calc.Pending,
  };

  let get_z = model => model.zipper |> Calc.get_value;

  type persistent = PersistentZipper.t;
  let persist = (f: 'p => 'q, model: t('p_k, 'p, 'p_a)) =>
    model |> get_z |> PersistentZipper.persist(f);
  let unpersist = (f, p) => p |> PersistentZipper.unpersist(f) |> mk;

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

  let trailing_hole_ctx = (ed: t('p_k, 'p, 'p_a), info_map: Statics.Map.t) => {
    let segment = Zipper.unselect_and_zip(ed |> get_z);
    let convex_grout = Segment.convex_grout(segment);
    // print_endline(String.concat("; ", List.map(Grout.show, convex_grout)));
    let last = Util.ListUtil.last_opt(convex_grout);
    switch (last) {
    | None => None
    | Some(grout) =>
      let id = grout.id;
      let info = Id.Map.find_opt(id, info_map);
      switch (info) {
      | Some(info) => Some(Info.ctx_of(info))
      | _ => None
      };
    };
  };

  let get_cached_term = editor => Calc.get_saved_exc(editor.term);
};

module Update = {
  open Calc.Syntax;
  type t('p_k, 'p, 'p_a) = Action.t('p_k, 'p, 'p_a);

  let update =
      (
        type p,
        type p_k,
        type p_a,
        ~settings: CoreSettings.t,
        ~projector_init,
        ~seg_of_projector,
        ~get_focusable,
        ~update_projector,
        ~livelit_projectors,
        a: Action.t(p_k, p, p_a),
        old_statics,
        model: Model.t(p_k, p, p_a),
      )
      : Action.Result.t(Model.t(p_k, p, p_a)) => {
    let seg_to_ed = seg => Zipper.unzip(seg) |> Model.mk |> Option.some;
    open Result.Syntax;
    // 1. Clear the autocomplete buffer if relevant
    let zipper =
      settings.assist && settings.statics && a != Buffer(Accept)
        ? Perform.go_z(
            ~settings,
            ~seg_to_ed,
            ~projector_init,
            ~seg_of_projector,
            ~get_focusable,
            ~update_projector,
            ~livelit_projectors,
            old_statics,
            Buffer(Clear),
            Model.to_move_s(model),
            model |> Model.get_z,
          )
          |> Action.Result.ok
          |> Option.value(~default=model |> Model.get_z)
        : model |> Model.get_z;
    let zipper =
      if (settings.assist && settings.statics && a != Buffer(Accept)) {
        Calc.NewValue(zipper);
      } else {
        Calc.OldValue(zipper);
      };
    let model = {
      ...model,
      zipper,
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
        ~get_focusable,
        ~update_projector,
        ~livelit_projectors,
        old_statics,
        a,
        Model.to_move_s(model),
        model |> Model.get_z,
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

    // 5. Trigger caret animation
    settings.flip_animations && Action.should_animate(a)
      ? Animation.request([Animation.Actions.move("caret")]) : ();

    model;
  };

  let make_term =
      (type p, ~make_term_prj, ~sort, model: Model.t('p_k, p, 'p_a)) => {
    let new_sort =
      Calc.set(sort, model.sort |> Calc.map_saved(Calc.get_value));

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
    let model = {
      ...model,
      term: term |> Calc.save,
      sort: Calc.Calculated(new_sort),
    };
    (model, term);
  };

  let calculate =
      (
        type p,
        ~common: ProjectorInterface.common,
        ~projector_init,
        ~projector_to_term,
        ~seg_of_projector,
        ~shape_of_projector,
        ~get_focusable,
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
              ~get_focusable,
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
          ~projector_to_term,
          ~shape_of_projector,
          ~info_map=common.statics.info_map,
          ~dyn_map=common.dynamics,
          ~sort=Calc.get_saved_exc(model.term) |> Any.sort,
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
      zipper: Calc.make_old(model.zipper),
      selection: Calc.make_old(model.selection),
      col_target: model.col_target,

      term: model.term,
      sort: model.sort |> Calc.map_saved(Calc.make_old),

      syntax: model.syntax,
      selection_ids: model.selection_ids,
    };
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t('p_k, 'p, 'p_a) = Model.t('p_k, 'p, 'p_a);
