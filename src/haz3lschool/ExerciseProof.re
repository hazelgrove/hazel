open Sexplib.Std;
open Haz3lcore;

module type ExerciseEnv = {
  type node;
  let default: node;
  let output_header: string => string;
};

module F = (ExerciseEnv: ExerciseEnv) => {
  open ExerciseBase.F(ExerciseEnv);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type derivation('code) = {
    jdmt: 'code,
    rule: Derivation.Rule.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type derivation_tree('code) = Util.Tree.t(derivation('code));

  // [@deriving (show({with_path: false}), sexp, yojson)]
  // type point_distribution = {
  //   test_validation: int,
  //   mutation_testing: int,
  //   impl_grading: int,
  // };

  // let validate_point_distribution =
  //     ({test_validation, mutation_testing, impl_grading}: point_distribution) =>
  //   test_validation + mutation_testing + impl_grading == 100
  //     ? () : failwith("Invalid point distribution in exercise.");

  // TODO: To be refactored
  // title, version, module_name, and prompt are shared by all exercise types

  [@deriving (show({with_path: false}), sexp, yojson)]
  type p('code) = {
    header,
    prelude: 'code,
    derivation_tree: derivation_tree('code),
  };

  let key_of = ({header, _}) => (header.title, header.version);

  let find_key_opt = (key, specs: list(p('code))) => {
    specs |> Util.ListUtil.findi_opt(spec => key_of(spec) == key);
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type pos =
    | Prelude
    | Derive(Util.Tree.pos);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type spec = p(Zipper.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type transitionary_spec = p(CodeString.t);

  let map = (f: 'a => 'b, p: p('a)): p('b) => {
    {
      ...p,
      prelude: p.prelude |> f,
      derivation_tree:
        p.derivation_tree
        |> Util.Tree.map(({jdmt, rule}) => {jdmt: jdmt |> f, rule}),
    };
  };

  let mapi = (f, p: p('a)): p('b) => {
    {
      header: p.header,
      prelude: f(Prelude, p.prelude),
      derivation_tree:
        p.derivation_tree
        |> Util.Tree.mapi((pos, {jdmt, rule}) => {
             {jdmt: f(Derive(pos), jdmt), rule}
           }),
    };
  };

  let nth = (p: p('a), pos: pos): 'a =>
    switch (pos) {
    | Prelude => p.prelude
    | Derive(pos) => Util.Tree.nth(p.derivation_tree, pos).jdmt
    };

  let map_nth = (f, p: p('a), pos: pos): p('a) =>
    switch (pos) {
    | Prelude => {...p, prelude: f(p.prelude)}
    | Derive(pos) => {
        ...p,
        derivation_tree:
          Util.Tree.map_nth(
            ({jdmt, rule}) => {jdmt: jdmt |> f, rule},
            p.derivation_tree,
            pos,
          ),
      }
    };

  let put_nth = (x: 'a, p: p('a), pos: pos): p('a) =>
    map_nth(Fun.const(x), p, pos);

  let flatten = p =>
    [p.prelude]
    @ (p.derivation_tree |> Util.Tree.map(d => d.jdmt) |> Util.Tree.flatten);

  // TODO: To be refactored
  [@deriving (show({with_path: false}), sexp, yojson)]
  type eds = p(Editor.t);

  // TODO: To be refactored
  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = {
    pos,
    eds,
  };

  // TODO: To be refactored
  let key_of_state = ({eds, _}) => key_of(eds);

  // TODO: To be refactored
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_state = (pos, list((pos, PersistentZipper.t)));

  let editor_of_state = ({pos, eds}: state): Editor.t => nth(eds, pos);

  let put_editor = ({pos, eds}: state, editor: Editor.t): state => {
    pos,
    eds: put_nth(editor, eds, pos),
  };

  let editors = ({eds, _}: state): list(Editor.t) => eds |> flatten;

  let editor_positions = ({eds, _}: state): list(pos) =>
    eds |> mapi((i, _) => i) |> flatten;

  let positioned_editors = state =>
    List.combine(editor_positions(state), editors(state));

  // let idx_of_pos = (pos, p: p('code)) =>
  //   switch (pos) {
  //   | Prelude => 0
  //   | CorrectImpl => 1
  //   | YourTestsTesting => 2
  //   | YourTestsValidation => 3
  //   | YourImpl => 4
  //   | HiddenBugs(i) =>
  //     if (i < List.length(p.hidden_bugs)) {
  //       5 + i;
  //     } else {
  //       failwith("invalid hidden bug index");
  //     }
  //   | HiddenTests => 5 + List.length(p.hidden_bugs)
  //   // Node(zhiyao): This could be wrong
  //   | Derive(_) => 5 + List.length(p.hidden_bugs) + 1
  //   };

  // let pos_of_idx = (p: p('code), idx: int) =>
  //   switch (idx) {
  //   | 0 => Prelude
  //   | 1 => CorrectImpl
  //   | 2 => YourTestsTesting
  //   | 3 => YourTestsValidation
  //   | 4 => YourImpl
  //   | _ =>
  //     if (idx < 0) {
  //       failwith("negative idx");
  //     } else if (idx < 5 + List.length(p.hidden_bugs)) {
  //       HiddenBugs(idx - 5);
  //     } else if (idx == 5 + List.length(p.hidden_bugs)) {
  //       HiddenTests;
  //                  // Node(zhiyao): This could be wrong
  //     } else {
  //       failwith("element idx");
  //     }
  //   };

  let switch_editor = (~pos, instructor_mode, ~exercise) => {
    ignore(instructor_mode); // TODO: settle instructor mode
    {...exercise, pos};
  };

  // TODO: To be refactored
  let zipper_of_code = code => {
    switch (Printer.zipper_of_string(code)) {
    | None => failwith("Transition failed.")
    | Some(zipper) => zipper
    };
  };

  let switch_derivation_rule = (~pos, ~new_rule, ~exercise) =>
    switch (pos) {
    | Derive(pos) =>
      let tree = exercise.eds.derivation_tree;
      let Node({jdmt, rule}, children) = Util.Tree.nth_node(tree, pos);
      let old_prems_num = Derivation.Rule.prem_num(rule);
      let new_prems_num = Derivation.Rule.prem_num(new_rule);
      let delta = new_prems_num - old_prems_num;
      let rec iter = (f, n, x) => n == 0 ? x : iter(f, n - 1, f(x));
      let children =
        if (delta > 0) {
          iter(
            fun
            | c => [
                Util.Tree.init(_ =>
                  {jdmt: Editor.init(zipper_of_code("")), rule: Assumption}
                ),
                ...c,
              ],
            delta,
            children,
          );
        } else {
          iter(
            fun
            | c => List.tl(c),
            - delta,
            children,
          );
        };
      let tree =
        Util.Tree.put_nth_node(
          Node({jdmt, rule: new_rule}, children),
          tree,
          pos,
        );
      {
        ...exercise,
        eds: {
          ...exercise.eds,
          derivation_tree: tree,
        },
      };
    | _ => exercise
    };

  let transition: transitionary_spec => spec = map(zipper_of_code);

  // TODO: To be refactored
  let editor_of_serialization = zipper => Editor.init(zipper);
  let eds_of_spec: spec => eds = map(editor_of_serialization);

  // TODO: setting instructor mode
  let set_instructor_mode = (state: state, new_mode: bool) => {
    ignore(new_mode);
    state;
  };

  // TODO: setting instructor mode
  let visible_in = (pos, ~instructor_mode) => {
    ignore(instructor_mode);
    ignore(pos);
    true;
  };

  // TODO: setting instructor mode
  let state_of_spec = (spec: spec, ~instructor_mode: bool): state => {
    ignore(instructor_mode);
    {pos: Prelude, eds: eds_of_spec(spec)};
    // set_instructor_mode({pos: YourImpl, eds}, instructor_mode);
  };

  // TODO: to be refactored
  let persistent_state_of_state =
      ({pos, _} as state: state, ~instructor_mode: bool) => {
    let zippers =
      positioned_editors(state)
      |> List.filter(((pos, _)) => visible_in(pos, ~instructor_mode))
      |> List.map(((pos, editor)) => {
           (pos, PersistentZipper.persist(Editor.(editor.state.zipper)))
         });
    (pos, zippers);
  };

  let unpersist_state =
      (
        (pos, positioned_zippers): persistent_state,
        ~spec: spec,
        ~instructor_mode: bool,
      )
      : state => {
    let lookup = (pos, default) =>
      if (visible_in(pos, ~instructor_mode)) {
        let persisted_zipper = List.assoc(pos, positioned_zippers);
        let zipper = PersistentZipper.unpersist(persisted_zipper);
        Editor.init(zipper);
      } else {
        editor_of_serialization(default);
      };
    let eds = spec |> mapi(lookup);
    set_instructor_mode({pos, eds}, instructor_mode);
  };

  // # Stitching

  type stitched('a) = {
    prelude: 'a, // prelude
    derivation_tree: Util.Tree.t('a) // prelude + your_impl + derivation
  };

  // TODO: to be refactored
  // let stitch3 = (ed1: Editor.t, ed2: Editor.t, ed3: Editor.t) =>
  //   EditorUtil.append_exp(
  //     EditorUtil.append_exp(term_of(ed1), term_of(ed2)),
  //     term_of(ed3),
  //   );

  let stitch_term = ({eds, _}: state): stitched(TermItem.t) => {
    let prelude_term =
      eds.prelude |> term_of |> wrap_filter(FilterAction.Eval);
    let derivation_tree =
      eds.derivation_tree
      |> Util.Tree.map(({jdmt, _}) =>
           wrap(jdmt |> term_of |> EditorUtil.append_exp(prelude_term), jdmt)
         );
    {prelude: wrap(prelude_term, eds.prelude), derivation_tree};
  };
  // TODO: to be refactored
  let stitch_term = Core.Memo.general(stitch_term);

  // TODO: to be refactored
  type stitched_statics = stitched(StaticsItem.t);

  // TODO: to be refactored
  /* Multiple stitchings are needed for each exercise
     (see comments in the stitched type above)

     Stitching is necessary to concatenate terms
     from different editors, which are then typechecked. */
  let stitch_static =
      (settings: CoreSettings.t, t: stitched(TermItem.t)): stitched_statics => {
    let mk = ({term, term_ranges, _}: TermItem.t): StaticsItem.t => {
      let info_map = Interface.Statics.mk_map(settings, term);
      {
        term,
        error_ids: Statics.Map.error_ids(term_ranges, info_map),
        info_map,
      };
    };
    {
      prelude: mk(t.prelude),
      derivation_tree: t.derivation_tree |> Util.Tree.map(mk),
    };
  };

  // TODO: to be refactored
  let stitch_static = Core.Memo.general(stitch_static);

  let statics_of_stiched =
      (state: state, s: stitched(StaticsItem.t)): StaticsItem.t =>
    switch (state.pos) {
    | Prelude => s.prelude
    | Derive(pos) => Util.Tree.nth(s.derivation_tree, pos)
    };

  // TODO: to be refactored
  let statics_of = (~settings, exercise: state): StaticsItem.t =>
    exercise
    |> stitch_term
    |> stitch_static(settings)
    |> statics_of_stiched(exercise);

  let prelude_key = "prelude";

  let derivation_tree_key = {
    let rec aux = (acc, pos: Util.Tree.pos) =>
      switch (pos) {
      | Value => acc
      | Children(i, pos) => aux(acc ++ "_" ++ string_of_int(i), pos)
      };
    aux("derivation");
  };

  let key_for_statics = (state: state): string =>
    switch (state.pos) {
    | Prelude => prelude_key
    | Derive(pos) => derivation_tree_key(pos)
    };

  // TODO: where should prelude_key be defined?
  let spliced_elabs =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, DHExp.t)) => {
    let {prelude: _, derivation_tree} =
      stitch_static(settings, stitch_term(state));
    let elab = (s: CachedStatics.statics) =>
      Interface.elaborate(~settings, s.info_map, s.term);
    derivation_tree
    |> Util.Tree.mapi((pos, si) => (derivation_tree_key(pos), elab(si)))
    |> Util.Tree.flatten;
  };

  let mk_statics =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, StaticsItem.t)) => {
    let stitched = stitch_static(settings, stitch_term(state));
    [(prelude_key, stitched.prelude)]
    @ (
      stitched.derivation_tree
      |> Util.Tree.mapi((pos, si) => (derivation_tree_key(pos), si))
      |> Util.Tree.flatten
    );
  };

  /* Given the evaluation results, collects the
     relevant information for producing dynamic
     feedback*/
  let stitch_dynamic =
      (
        settings: CoreSettings.t,
        state: state,
        results: option(ModelResults.t),
      )
      : stitched(DynamicsItem.t) => {
    let {prelude, derivation_tree} =
      stitch_static(settings, stitch_term(state));
    let result_of = key =>
      switch (results) {
      | None => ModelResult.NoElab
      | Some(results) =>
        ModelResults.lookup(results, key)
        |> Option.value(~default=ModelResult.NoElab)
      };

    let prelude =
      DynamicsItem.{
        term: prelude.term,
        info_map: prelude.info_map,
        result: NoElab,
      };
    let derivation_tree =
      derivation_tree
      |> Util.Tree.mapi((pos, statics_item: StaticsItem.t) =>
           DynamicsItem.{
             term: statics_item.term,
             info_map: statics_item.info_map,
             result: result_of(derivation_tree_key(pos)),
           }
         );
    {prelude, derivation_tree};
  };

  let stitch_dynamic =
      (
        settings: CoreSettings.t,
        state: state,
        results: option(ModelResults.t),
      )
      : stitched(DynamicsItem.t) =>
    if (settings.statics && settings.dynamics) {
      stitch_dynamic(settings, state, results);
    } else if (settings.statics) {
      let t = stitch_static(settings, stitch_term(state));
      {
        prelude: DynamicsItem.statics_only(t.prelude),
        derivation_tree:
          Util.Tree.map(DynamicsItem.statics_only, t.derivation_tree),
      };
    } else {
      {
        prelude: DynamicsItem.empty,
        derivation_tree:
          state.eds.derivation_tree |> Util.Tree.map(_ => DynamicsItem.empty),
      };
    };
};
