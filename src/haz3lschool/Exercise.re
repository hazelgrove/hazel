open Sexplib.Std;
open Haz3lcore;

module type ExerciseEnv = {
  type node;
  let default: node;
  let output_header: string => string;
};

let output_header_grading = _module_name =>
  "module Exercise = GradePrelude.Exercise\n" ++ "let prompt = ()\n";

module F = (ExerciseEnv: ExerciseEnv) => {
  module Programming = ExerciseProgramming.Model;
  module Proof = ExerciseProof.Model;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type header = {
    title: string,
    version: int,
    module_name: string,
    prompt:
      [@printer (fmt, _) => Format.pp_print_string(fmt, "prompt")] [@opaque] ExerciseEnv.node,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model('code) =
    | Programming(Programming.p('code))
    | Proof(Proof.p('code));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type pos =
    | Programming(Programming.pos)
    | Proof(Proof.pos);

  // Utility Functions
  let map = (f: 'a => 'b, m: model('a)): model('b) =>
    switch (m) {
    | Programming(m) => Programming(Programming.map(f, m))
    | Proof(m) => Proof(Proof.map(f, m))
    };

  let mapi = (f: (pos, 'a) => 'b, m: model('a)): model('b) =>
    switch (m) {
    | Programming(m) =>
      Programming(Programming.mapi(pos => Programming(pos) |> f, m))
    | Proof(m) => Proof(Proof.mapi(pos => Proof(pos) |> f, m))
    };

  let nth = (m: model('a), pos: pos): 'a =>
    switch (pos, m) {
    | (Programming(pos), Programming(m)) => Programming.nth(m, pos)
    | (Proof(pos), Proof(m)) => Proof.nth(m, pos)
    | _ => failwith("Exercise(nth): mismatch")
    };

  let map_nth = (f: 'a => 'a, m: model('a), pos: pos): model('a) => {
    switch (pos, m) {
    | (Programming(pos), Programming(m)) =>
      Programming(Programming.map_nth(f, m, pos))
    | (Proof(pos), Proof(m)) => Proof(Proof.map_nth(f, m, pos))
    | _ => failwith("Exercise(map_nth): position mismatch")
    };
  };

  let flatten = (m: model('a)) =>
    switch (m) {
    | Programming(m) => Programming.flatten(m)
    | Proof(m) => Proof.flatten(m)
    };

  // Exported Functions
  [@deriving (show({with_path: false}), sexp, yojson)]
  type p('code) = {
    header,
    pos,
    model: model('code),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type spec = p(Zipper.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type transitionary_spec = p(CodeString.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type state = p(Editor.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_state = (pos, list((pos, PersistentZipper.t)));

  let editor_of_state = ({pos, model, _}: state): Editor.t =>
    nth(model, pos);

  let put_editor = ({pos, model, _} as state: state, editor: Editor.t): state => {
    ...state,
    model: map_nth(editor |> Fun.const, model, pos),
  };

  let editors = ({model, _}: state): list(Editor.t) => model |> flatten;

  let editor_positions = ({model, _}: state): list(pos) =>
    model |> mapi((i, _) => i) |> flatten;

  let positioned_editors = state =>
    List.combine(editor_positions(state), editors(state));

  let zipper_of_code = code => {
    switch (Printer.zipper_of_string(code)) {
    | None => failwith("Transition failed.")
    | Some(zipper) => zipper
    };
  };

  let transition: transitionary_spec => spec =
    s => {...s, model: s.model |> map(zipper_of_code)};

  let editor_of_serialization: Zipper.t => Editor.t =
    zipper => Editor.init(zipper);

  let eds_of_spec: spec => model(Editor.t) =
    ({model, _}) => model |> map(editor_of_serialization);

  let switch_editor = (~pos: pos, instructor_mode, ~state: state) =>
    if (switch (pos) {
        | Programming(pos) => Programming.switch_editor(~pos, instructor_mode)
        | Proof(pos) => Proof.switch_editor(~pos, instructor_mode)
        }) {
      {...state, pos};
    } else {
      state;
    };

  let switch_derivation_rule =
      (~pos: Util.Tree.pos, ~exercise: state, ~rule: Derivation.Rule.t): state =>
    switch (exercise.model) {
    | Proof(m) =>
      let m = Proof.switch_derivation_rule(~pos, ~rule, ~m);
      {...exercise, pos: Proof(Derive(pos)), model: Proof(m)};
    | _ => exercise
    };

  let add_premise =
      (~pos: Util.Tree.pos, ~index: int, ~exercise: state): state =>
    switch (exercise.model) {
    | Proof(m) =>
      let jdmt = "" |> zipper_of_code |> Editor.init;
      let rule = Derivation.Rule.Assumption;
      let init = {Proof.jdmt, rule} |> Fun.const;
      let m = Proof.add_premise(~pos, ~index, ~m, ~init);
      {...exercise, pos: Proof(Derive(pos)), model: Proof(m)};
    | _ => exercise
    };

  let del_premise =
      (~pos: Util.Tree.pos, ~index: int, ~exercise: state): state =>
    switch (exercise.model) {
    | Proof(m) =>
      let m = Proof.del_premise(~pos, ~index, ~m);
      {...exercise, pos: Proof(Derive(pos)), model: Proof(m)};
    | _ => exercise
    };

  let set_instructor_mode = (exercise: state, ~new_mode: bool) => {
    ...exercise,
    model:
      mapi(
        (pos, editor) =>
          Editor.set_read_only(
            editor,
            switch (pos) {
            | Programming(pos) => Programming.readonly_in(pos, new_mode)
            | Proof(pos) => Proof.readonly_in(pos, new_mode)
            },
          ),
        exercise.model,
      ),
  };

  let visible_in = (pos, ~instructor_mode) =>
    switch (pos) {
    | Programming(pos) => Programming.visible_in(pos, instructor_mode)
    | Proof(pos) => Proof.visible_in(pos, instructor_mode)
    };

  let state_of_spec = (spec: spec, ~instructor_mode: bool): state =>
    {...spec, model: eds_of_spec(spec)}
    |> set_instructor_mode(~new_mode=instructor_mode);

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
    set_instructor_mode(
      {header: spec.header, pos, model: spec.model |> mapi(lookup)},
      ~new_mode=instructor_mode,
    );
  };

  // Module Export

  let editor_pp = (fmt, editor: Editor.t) => {
    let zipper = editor.state.zipper;
    let serialization = Zipper.show(zipper);
    // let string_literal = "\"" ++ String.escaped(serialization) ++ "\"";
    Format.pp_print_string(fmt, serialization);
  };

  let export_module = (module_name, {model, _}: state) => {
    let prefix =
      "let prompt = "
      ++ module_name
      ++ "_prompt.prompt\n"
      ++ "let exercise: Exercise.spec = ";
    let record = show_model(editor_pp, model);
    prefix ++ record ++ "\n";
  };

  let transitionary_editor_pp = (fmt, editor: Editor.t) => {
    let zipper = editor.state.zipper;
    let code = Printer.to_string_basic(zipper);
    Format.pp_print_string(fmt, "\"" ++ String.escaped(code) ++ "\"");
  };

  let export_transitionary_module = (module_name, {model, _}: state) => {
    let prefix =
      "let prompt = "
      ++ module_name
      ++ "_prompt.prompt\n"
      ++ "let exercise: Exercise.spec = Exercise.transition(";
    let record = show_model(transitionary_editor_pp, model);
    prefix ++ record ++ ")\n";
  };

  let export_grading_module = (module_name, {model, _}: state) => {
    let header = output_header_grading(module_name);
    let prefix = "let exercise: Exercise.spec = ";
    let record = show_model(editor_pp, model);
    header ++ prefix ++ record ++ "\n";
  };

  // From Store

  [@deriving (show({with_path: false}), sexp, yojson)]
  type key = (string, int); // title, version

  let key: p('a) => key = ({header: h, _}) => (h.title, h.version);

  let find_key_opt = (k, specs: list(p('code))) =>
    specs |> Util.ListUtil.findi_opt(spec => key(spec) == k);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type exercise_export = {
    cur_exercise: key,
    exercise_data: list((key, persistent_state)),
  };

  let serialize_exercise = (exercise, ~instructor_mode) => {
    persistent_state_of_state(exercise, ~instructor_mode)
    |> sexp_of_persistent_state
    |> Sexplib.Sexp.to_string;
  };

  let deserialize_exercise = (data, ~spec, ~instructor_mode) => {
    data
    |> Sexplib.Sexp.of_string
    |> persistent_state_of_sexp
    |> unpersist_state(~spec, ~instructor_mode);
  };

  let deserialize_exercise_export = data => {
    data |> Sexplib.Sexp.of_string |> exercise_export_of_sexp;
  };

  // # Stitching
  module ProgrammingStitch = ExerciseProgramming.Stitch;
  module ProofStitch = ExerciseProof.Stitch;

  type stitched('a) =
    | Programming(ProgrammingStitch.p('a))
    | Proof(ProofStitch.p('a));

  module TermItem = {
    type t = {
      term: TermBase.UExp.t,
      term_ranges: TermRanges.t,
    };
  };

  module StaticsItem = {
    type t = CachedStatics.statics;
  };

  module DynamicsItem = {
    type t = {
      term: TermBase.UExp.t,
      info_map: Statics.Map.t,
      result: ModelResult.t,
    };
    let empty: t = {
      term: {
        term: Tuple([]),
        ids: [Id.mk()],
      },
      info_map: Id.Map.empty,
      result: NoElab,
    };
    let statics_only = ({term, info_map, _}: StaticsItem.t): t => {
      {term, info_map, result: NoElab};
    };
  };

  let wrap_filter = (act: FilterAction.action, term: Term.UExp.t): Term.UExp.t =>
    TermBase.UExp.{
      term:
        TermBase.UExp.Filter(
          FilterAction.(act, One),
          {term: Constructor("$e"), ids: [Id.mk()]},
          term,
        ),
      ids: [Id.mk()],
    };

  let wrap = (term, editor: Editor.t): TermItem.t => {
    term,
    term_ranges: editor.state.meta.term_ranges,
  };

  let term_of = (editor: Editor.t): Term.UExp.t =>
    editor.state.meta.view_term;

  let stitch3 = (ed1: Editor.t, ed2: Editor.t, ed3: Editor.t) =>
    EditorUtil.append_exp(
      EditorUtil.append_exp(term_of(ed1), term_of(ed2)),
      term_of(ed3),
    );

  let stitch_term = ({model, _}: state): stitched(TermItem.t) =>
    switch (model) {
    | Proof(model) =>
      let prelude_term =
        model.prelude |> term_of |> wrap_filter(FilterAction.Eval);
      let derivation_tree =
        model.derivation_tree
        |> Util.Tree.map(({Proof.jdmt, _}) =>
             wrap(
               jdmt |> term_of |> EditorUtil.append_exp(prelude_term),
               jdmt,
             )
           );
      Proof({prelude: wrap(prelude_term, model.prelude), derivation_tree});
    | Programming(model) =>
      let instructor =
        stitch3(model.prelude, model.correct_impl, model.hidden_tests.tests);
      let user_impl_term = {
        let your_impl_term =
          model.your_impl |> term_of |> wrap_filter(FilterAction.Step);
        let prelude_term =
          model.prelude |> term_of |> wrap_filter(FilterAction.Eval);
        EditorUtil.append_exp(prelude_term, your_impl_term);
      };
      let test_validation_term =
        stitch3(model.prelude, model.correct_impl, model.your_tests.tests);
      let user_tests_term =
        EditorUtil.append_exp(
          user_impl_term,
          term_of(model.your_tests.tests),
        );
      let hidden_tests_term =
        EditorUtil.append_exp(
          user_impl_term,
          term_of(model.hidden_tests.tests),
        );
      Programming({
        test_validation: wrap(test_validation_term, model.your_tests.tests),
        user_impl: wrap(user_impl_term, model.your_impl),
        user_tests: wrap(user_tests_term, model.your_tests.tests),
        // instructor works here as long as you don't shadow anything in the prelude
        prelude: wrap(instructor, model.prelude),
        instructor: wrap(instructor, model.correct_impl),
        hidden_bugs:
          List.map(
            (t: Programming.wrong_impl('a)): TermItem.t =>
              wrap(
                stitch3(model.prelude, t.impl, model.your_tests.tests),
                t.impl,
              ),
            model.hidden_bugs,
          ),
        hidden_tests: wrap(hidden_tests_term, model.hidden_tests.tests),
      });
    };
  let stitch_term = Core.Memo.general(stitch_term);

  type stitched_statics = stitched(StaticsItem.t);

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
    switch (t) {
    // Prelude should be `instructor`
    | Programming(t) => Programming(ProgrammingStitch.map(mk, t))
    | Proof(t) => Proof(ProofStitch.map(mk, t))
    };
  };
  let stitch_static = Core.Memo.general(stitch_static);

  let statics_of_stiched =
      (state: state, s: stitched(StaticsItem.t)): StaticsItem.t =>
    switch (state.pos, s) {
    | (Programming(pos), Programming(s)) => ProgrammingStitch.nth(s, pos)
    | (Proof(pos), Proof(s)) => ProofStitch.nth(s, pos)
    | _ => failwith("Exercise(statics_of_stiched): position mismatch")
    };

  let statics_of = (~settings, exercise: state): StaticsItem.t =>
    exercise
    |> stitch_term
    |> stitch_static(settings)
    |> statics_of_stiched(exercise);

  let key_for_statics = (state: state): string =>
    switch (state.pos) {
    | Programming(pos) => ProgrammingStitch.key(pos)
    | Proof(pos) => ProofStitch.key(pos)
    };

  let spliced_elabs =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, DHExp.t)) => {
    let elab = (s: CachedStatics.statics) =>
      Interface.elaborate(~settings, s.info_map, s.term);
    switch (stitch_static(settings, stitch_term(state))) {
    // Note: prelude should be filtered
    | Programming(stitched) =>
      ProgrammingStitch.mapi(
        (pos, si) => (ProgrammingStitch.key(pos), elab(si)),
        stitched,
      )
      |> ProgrammingStitch.flatten
    | Proof(stitched) =>
      ProofStitch.mapi(
        (pos, si) => (ProofStitch.key(pos), elab(si)),
        stitched,
      )
      |> ProofStitch.flatten
    };
  };

  let mk_statics =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, StaticsItem.t)) =>
    switch (stitch_static(settings, stitch_term(state))) {
    | Programming(stitched) =>
      ProgrammingStitch.mapi(
        (pos, si) => (ProgrammingStitch.key(pos), si),
        stitched,
      )
      |> ProgrammingStitch.flatten
    | Proof(stitched) =>
      ProofStitch.mapi((pos, si) => (ProofStitch.key(pos), si), stitched)
      |> ProofStitch.flatten
    };

  let stitch_dynamic =
      (
        settings: CoreSettings.t,
        state: state,
        results: option(ModelResults.t),
      )
      : stitched(DynamicsItem.t) => {
    let result_of = key =>
      switch (results) {
      | None => ModelResult.NoElab
      | Some(results) =>
        ModelResults.lookup(results, key)
        |> Option.value(~default=ModelResult.NoElab)
      };
    switch (stitch_static(settings, stitch_term(state))) {
    // Prelude should be NoElab
    | Programming(stitched) =>
      Programming(
        ProgrammingStitch.mapi(
          (pos, si: StaticsItem.t) =>
            DynamicsItem.{
              term: si.term,
              info_map: si.info_map,
              result: result_of(ProgrammingStitch.key(pos)),
            },
          stitched,
        ),
      )
    | Proof(stitched) =>
      Proof(
        ProofStitch.mapi(
          (pos, si: StaticsItem.t) =>
            DynamicsItem.{
              term: si.term,
              info_map: si.info_map,
              result: result_of(ProofStitch.key(pos)),
            },
          stitched,
        ),
      )
    };
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
      : stitched(DynamicsItem.t) =>
    if (settings.statics && settings.dynamics) {
      stitch_dynamic(settings, state, results);
    } else if (settings.statics) {
      switch (stitch_static(settings, stitch_term(state))) {
      | Programming(stitched) =>
        Programming(
          ProgrammingStitch.map(DynamicsItem.statics_only, stitched),
        )
      | Proof(stitched) =>
        Proof(ProofStitch.map(DynamicsItem.statics_only, stitched))
      };
    } else {
      switch (state.model) {
      | Programming(model) =>
        Programming({
          test_validation: DynamicsItem.empty,
          user_impl: DynamicsItem.empty,
          user_tests: DynamicsItem.empty,
          instructor: DynamicsItem.empty,
          prelude: DynamicsItem.empty,
          hidden_bugs:
            List.init(List.length(model.hidden_bugs), _ =>
              DynamicsItem.empty
            ),
          hidden_tests: DynamicsItem.empty,
        })
      | Proof(content) =>
        Proof({
          prelude: DynamicsItem.empty,
          derivation_tree:
            content.derivation_tree |> Util.Tree.map(_ => DynamicsItem.empty),
        })
      };
    };

  let stitch_dynamic = Core.Memo.general(stitch_dynamic);
};
