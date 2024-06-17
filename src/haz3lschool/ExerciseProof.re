open Sexplib.Std;
open Haz3lcore;

module type ExerciseEnv = {
  type node;
  let default: node;
  let output_header: string => string;
};

module F = (ExerciseEnv: ExerciseEnv) => {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type derivation('code) = {
    jdmt: 'code,
    rule: Derivation.Rule.t,
  };

  let map_jdmt = (f, {jdmt, rule}) => {jdmt: f(jdmt), rule};

  let put_jdmt = (jdmt, {rule, _}) => {jdmt, rule};

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
    title: string,
    version: int,
    module_name: string,
    prompt:
      [@printer (fmt, _) => Format.pp_print_string(fmt, "prompt")] [@opaque] ExerciseEnv.node,
    // point_distribution,
    prelude: 'code,
    derivation_tree: derivation_tree('code),
  };

  let key_of = p => {
    (p.title, p.version);
  };

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

  let map = (p: p('a), f: 'a => 'b): p('b) => {
    {
      title: p.title,
      version: p.version,
      module_name: p.module_name,
      prompt: p.prompt,
      // point_distribution: p.point_distribution,
      prelude: f(p.prelude),
      derivation_tree: p.derivation_tree |> Util.Tree.map(f |> map_jdmt),
    };
  };

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

  let editor_of_state: state => Editor.t =
    ({pos, eds, _}) =>
      switch (pos) {
      | Prelude => eds.prelude
      | Derive(pos) => Util.Tree.nth(eds.derivation_tree, pos).jdmt
      };

  let put_editor = ({pos, eds, _} as state: state, editor: Editor.t) =>
    switch (pos) {
    | Prelude => {
        ...state,
        eds: {
          ...eds,
          prelude: editor,
        },
      }
    | Derive(pos) => {
        ...state,
        eds: {
          ...eds,
          derivation_tree:
            Util.Tree.map_nth(put_jdmt(editor), eds.derivation_tree, pos),
        },
      }
    };

  let editors = ({eds, _}: state) =>
    [eds.prelude]
    @ (eds.derivation_tree |> Util.Tree.map(d => d.jdmt) |> Util.Tree.flatten);

  let editor_positions = ({eds, _}: state) =>
    [Prelude]
    @ (
      eds.derivation_tree
      |> Util.Tree.flatten_pos
      |> List.map(pos => Derive(pos))
    );

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
    {eds: exercise.eds, pos};
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

  let transition: transitionary_spec => spec =
    (
      {
        title,
        version,
        module_name,
        prompt,
        // point_distribution,
        prelude,
        derivation_tree,
      },
    ) => {
      let prelude = zipper_of_code(prelude);
      let derivation_tree =
        derivation_tree |> Util.Tree.map(zipper_of_code |> map_jdmt);
      {
        title,
        version,
        module_name,
        prompt,
        // point_distribution,
        prelude,
        derivation_tree,
      };
    };

  // TODO: To be refactored
  let editor_of_serialization = zipper => Editor.init(zipper);
  let eds_of_spec: spec => eds =
    (
      {
        title,
        version,
        module_name,
        prompt,
        // point_distribution,
        prelude,
        derivation_tree,
      },
    ) => {
      let prelude = editor_of_serialization(prelude);
      let derivation_tree =
        derivation_tree |> Util.Tree.map(editor_of_serialization |> map_jdmt);
      {
        title,
        version,
        module_name,
        prompt,
        // point_distribution,
        prelude,
        derivation_tree,
      };
    };

  //
  // Old version of above that did string-based parsing, may be useful
  // for transitions between zipper data structure versions (TODO)
  //
  // let editor_of_code = (init_id, code) =>
  //   switch (EditorUtil.editor_of_code(init_id, code)) {
  //   | None => failwith("Exercise error: invalid code")
  //   | Some(x) => x
  //   };
  // let eds_of_spec: spec => eds =
  //   (
  //     {
  //
  //       title,
  //       version,
  //       prompt,
  //       point_distribution,
  //       prelude,
  //       correct_impl,
  //       your_tests,
  //       your_impl,
  //       hidden_bugs,
  //       hidden_tests,
  //     },
  //   ) => {
  //     let id = next_id;
  //     let (id, prelude) = editor_of_code(id, prelude);
  //     let (id, correct_impl) = editor_of_code(id, correct_impl);
  //     let (id, your_tests) = {
  //       let (id, tests) = editor_of_code(id, your_tests.tests);
  //       (
  //         id,
  //         {
  //           tests,
  //           num_required: your_tests.num_required,
  //           minimum: your_tests.minimum,
  //         },
  //       );
  //     };
  //     let (id, your_impl) = editor_of_code(id, your_impl);
  //     let (id, hidden_bugs) =
  //       List.fold_left(
  //         ((id, acc), {impl, hint}) => {
  //           let (id, impl) = editor_of_code(id, impl);
  //           (id, acc @ [{impl, hint}]);
  //         },
  //         (id, []),
  //         hidden_bugs,
  //       );
  //     let (id, hidden_tests) = {
  //       let {tests, hints} = hidden_tests;
  //       let (id, tests) = editor_of_code(id, tests);
  //       (id, {tests, hints});
  //     };
  //     {
  //       next_id: id,
  //       title,
  //       version,
  //       prompt,
  //       point_distribution,
  //       prelude,
  //       correct_impl,
  //       your_tests,
  //       your_impl,
  //       hidden_bugs,
  //       hidden_tests,
  //     };
  //   };

  // TODO: setting instructor mode
  let set_instructor_mode = ({eds, _} as state: state, new_mode: bool) => {
    ...state,
    eds: {
      ...eds,
      prelude: Editor.set_read_only(eds.prelude, !new_mode),
    },
  };

  // TODO: setting instructor mode
  let visible_in = (pos, ~instructor_mode) => {
    ignore(instructor_mode);
    ignore(pos);
    true;
  };

  // TODO: setting instructor mode
  let state_of_spec = (spec, ~instructor_mode: bool): state => {
    let eds = eds_of_spec(spec);
    ignore(instructor_mode);
    {pos: Prelude, eds};
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
    let prelude = lookup(Prelude, spec.prelude);
    let derivation_tree =
      spec.derivation_tree
      |> Util.Tree.mapi((pos, {jdmt, rule}) => {
           let jdmt = lookup(Derive(pos), jdmt);
           {jdmt, rule};
         });
    set_instructor_mode(
      {
        pos,
        eds: {
          title: spec.title,
          version: spec.version,
          module_name: spec.module_name,
          prompt: spec.prompt,
          // point_distribution: spec.point_distribution,
          prelude,
          derivation_tree,
        },
      },
      instructor_mode,
    );
  };

  // # Stitching
  open ExerciseBase;

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
