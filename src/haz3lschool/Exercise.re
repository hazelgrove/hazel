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

  module ModelUtil = {
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
    ModelUtil.nth(model, pos);

  let put_editor = ({pos, model, _} as state: state, editor: Editor.t): state => {
    ...state,
    model: ModelUtil.map_nth(editor |> Fun.const, model, pos),
  };

  let editors = ({model, _}: state): list(Editor.t) =>
    model |> ModelUtil.flatten;

  let editor_positions = ({model, _}: state): list(pos) =>
    model |> ModelUtil.mapi((i, _) => i) |> ModelUtil.flatten;

  let positioned_editors = state =>
    List.combine(editor_positions(state), editors(state));

  let zipper_of_code = code => {
    switch (Printer.zipper_of_string(code)) {
    | None => failwith("Transition failed.")
    | Some(zipper) => zipper
    };
  };

  let transition: transitionary_spec => spec =
    s => {...s, model: s.model |> ModelUtil.map(zipper_of_code)};

  let editor_of_serialization: Zipper.t => Editor.t =
    zipper => Editor.init(zipper);

  let eds_of_spec: spec => model(Editor.t) =
    ({model, _}) => model |> ModelUtil.map(editor_of_serialization);

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
      let init = "" |> zipper_of_code |> Editor.init |> Fun.const;
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
      ModelUtil.mapi(
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
      {
        header: spec.header,
        pos,
        model: spec.model |> ModelUtil.mapi(lookup),
      },
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

  let serialize_exercise = (exercise, ~instructor_mode) =>
    persistent_state_of_state(exercise, ~instructor_mode)
    |> sexp_of_persistent_state
    |> Sexplib.Sexp.to_string;

  let deserialize_exercise = (data, ~spec, ~instructor_mode) =>
    data
    |> Sexplib.Sexp.of_string
    |> persistent_state_of_sexp
    |> unpersist_state(~spec, ~instructor_mode);

  let deserialize_exercise_export = data =>
    data |> Sexplib.Sexp.of_string |> exercise_export_of_sexp;

  // # Stitching
  module ProgrammingStitch = ExerciseProgramming.Stitch;
  module ProofStitch = ExerciseProof.Stitch;

  type stitched('a) =
    | Programming(ProgrammingStitch.p('a))
    | Proof(ProofStitch.p('a));

  module StitchUtil = {
    let map = (f: 'a => 'b, s: stitched('a)): stitched('b) =>
      switch (s) {
      | Programming(s) => Programming(ProgrammingStitch.map(f, s))
      | Proof(s) => Proof(ProofStitch.map(f, s))
      };

    let mapi = (f: (pos, 'a) => 'b, s: stitched('a)): stitched('b) =>
      switch (s) {
      | Programming(s) =>
        Programming(
          ProgrammingStitch.mapi((pos, x) => f(Programming(pos), x), s),
        )
      | Proof(s) =>
        Proof(ProofStitch.mapi((pos, x) => f(Proof(pos), x), s))
      };

    let nth = (s: stitched('a), pos: pos): 'a =>
      switch (pos, s) {
      | (Programming(pos), Programming(s)) => ProgrammingStitch.nth(s, pos)
      | (Proof(pos), Proof(s)) => ProofStitch.nth(s, pos)
      | _ => failwith("Exercise(StitchUtil.nth): mismatch")
      };

    let flatten = (s: stitched('a)) =>
      switch (s) {
      | Programming(s) => ProgrammingStitch.flatten(s)
      | Proof(s) => ProofStitch.flatten(s)
      };

    let key = (pos: pos): string =>
      switch (pos) {
      | Programming(pos) => ProgrammingStitch.key(pos)
      | Proof(pos) => ProofStitch.key(pos)
      };

    let fill = (m: model('a), init: pos => 'b): stitched('b) =>
      switch (m) {
      | Programming(m) =>
        Programming(
          ProgrammingStitch.fill(m, pos => Programming(pos) |> init),
        )
      | Proof(m) => Proof(ProofStitch.fill(m, pos => Proof(pos) |> init))
      };
  };

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

  let stitch_term = ({model, _}: state): stitched(TermItem.t) => {
    let term_of = (editor: Editor.t): Term.UExp.t =>
      MakeTerm.from_zip_for_sem(editor.state.zipper) |> fst;
    let stitch2 = (ed1: Editor.t, ed2: Editor.t) =>
      EditorUtil.append_exp(term_of(ed1), term_of(ed2));
    let stitch3 = (ed1: Editor.t, ed2: Editor.t, ed3: Editor.t) =>
      EditorUtil.append_exp(stitch2(ed1, ed2), term_of(ed3));
    let wrap_filter =
        (act: FilterAction.action, term: Term.UExp.t): Term.UExp.t =>
      TermBase.UExp.{
        term:
          TermBase.UExp.Filter(
            FilterAction.(act, One),
            {term: Constructor("$e"), ids: [Id.mk()]},
            term,
          ),
        ids: [Id.mk()],
      };
    let terms: stitched(TermBase.UExp.t) =
      switch (model) {
      | Proof(model) =>
        let prelude_term =
          model.prelude |> term_of |> wrap_filter(FilterAction.Eval);
        Proof({
          prelude: prelude_term,
          derivation_tree:
            model.derivation_tree
            |> Util.Tree.map(({Proof.jdmt, _}) =>
                 jdmt |> term_of |> EditorUtil.append_exp(prelude_term)
               ),
        });
      | Programming(model) =>
        let instructor_term =
          stitch3(
            model.prelude,
            model.correct_impl,
            model.hidden_tests.tests,
          );
        let your_impl_term =
          model.your_impl |> term_of |> wrap_filter(FilterAction.Step);
        let prelude_term =
          model.prelude |> term_of |> wrap_filter(FilterAction.Eval);
        let user_impl_term =
          EditorUtil.append_exp(prelude_term, your_impl_term);
        Programming({
          test_validation:
            stitch3(
              model.prelude,
              model.correct_impl,
              model.your_tests.tests,
            ),
          user_impl: user_impl_term,
          user_tests: stitch2(model.prelude, model.your_tests.tests),
          prelude: instructor_term,
          instructor: instructor_term,
          hidden_bugs:
            List.map(
              (t: Programming.wrong_impl('a)) =>
                stitch3(model.prelude, t.impl, model.your_tests.tests),
              model.hidden_bugs,
            ),
          hidden_tests: stitch2(model.prelude, model.hidden_tests.tests),
        });
      };
    let wrap = (term, editor: Editor.t): TermItem.t => {
      term,
      term_ranges: editor.state.meta.projected.term_ranges,
    };
    terms |> StitchUtil.mapi((pos, t) => wrap(t, ModelUtil.nth(model, pos)));
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
    // Prelude should be `instructor`
    StitchUtil.map(mk, t);
  };
  let stitch_static = Core.Memo.general(stitch_static);

  let statics_of = (~settings, exercise: state): StaticsItem.t =>
    StitchUtil.nth(
      exercise |> stitch_term |> stitch_static(settings),
      exercise.pos,
    );

  let key_for_statics = (state: state): string => StitchUtil.key(state.pos);

  let spliced_elabs =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, DHExp.t)) => {
    let elab = (s: CachedStatics.statics) =>
      Interface.elaborate(~settings, s.info_map, s.term);
    // Note: prelude should be filtered
    stitch_static(settings, stitch_term(state))
    |> StitchUtil.mapi((pos, si) => (StitchUtil.key(pos), elab(si)))
    |> StitchUtil.flatten;
  };

  let mk_statics =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, StaticsItem.t)) =>
    stitch_static(settings, stitch_term(state))
    |> StitchUtil.mapi((pos, si) => (StitchUtil.key(pos), si))
    |> StitchUtil.flatten;

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
    stitch_static(settings, stitch_term(state))
    |> StitchUtil.mapi((pos, si: StaticsItem.t) =>
         DynamicsItem.{
           term: si.term,
           info_map: si.info_map,
           result: result_of(StitchUtil.key(pos)),
         }
       );
    // Prelude should be NoElab
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
      stitch_static(settings, stitch_term(state))
      |> StitchUtil.map((si: StaticsItem.t) => DynamicsItem.statics_only(si));
    } else {
      StitchUtil.fill(state.model, DynamicsItem.empty |> Fun.const);
    };

  let stitch_dynamic = Core.Memo.general(stitch_dynamic);
};
