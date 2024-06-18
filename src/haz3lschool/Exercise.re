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
  module Programming = ExerciseProgramming.F(ExerciseEnv);
  module Proof = ExerciseProof.F(ExerciseEnv);
  include ExerciseBase.F(ExerciseEnv);

  type mode =
    | Programming
    | Proof;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type p('code) =
    | Programming(Programming.p('code))
    | Proof(Proof.p('code));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type key = (string, int); // title, version

  let key_of =
    fun
    | Programming(spec) => Programming.key_of(spec)
    | Proof(spec) => Proof.key_of(spec);

  let find_key_opt = (key, specs: list(p('code))) =>
    specs |> Util.ListUtil.findi_opt(spec => key_of(spec) == key);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type pos =
    | Programming(Programming.pos)
    | Proof(Proof.pos);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type spec = p(Zipper.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type transitionary_spec = p(CodeString.t);

  let map = (f: 'a => 'b, p: p('a)): p('b) =>
    switch (p) {
    | Programming(spec) => Programming(Programming.map(f, spec))
    | Proof(spec) => Proof(Proof.map(f, spec))
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type eds = p(Editor.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type state =
    | Programming(Programming.state)
    | Proof(Proof.state);

  let key_of_state =
    fun
    | Programming(state) => Programming.key_of_state(state)
    | Proof(state) => Proof.key_of_state(state);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent_state = (pos, list((pos, PersistentZipper.t)));

  let editor_of_state: state => Editor.t =
    fun
    | Programming(state) => Programming.editor_of_state(state)
    | Proof(state) => Proof.editor_of_state(state);

  let put_editor = (state: state, editor: Editor.t) =>
    switch (state) {
    | Programming(state) =>
      Programming(Programming.put_editor(state, editor))
    | Proof(state) => Proof(Proof.put_editor(state, editor))
    };

  let editors = (state: state) =>
    switch (state) {
    | Programming(state) => Programming.editors(state)
    | Proof(state) => Proof.editors(state)
    };

  let editor_positions = (state: state): list(pos) =>
    switch (state) {
    | Programming(state) =>
      Programming.editor_positions(state)
      |> List.map((ed) => (Programming(ed): pos))
    | Proof(state) =>
      Proof.editor_positions(state) |> List.map((ed) => (Proof(ed): pos))
    };

  let positioned_editors = state =>
    List.combine(editor_positions(state), editors(state));

  let switch_editor = (~pos: pos, instructor_mode, ~exercise: state) =>
    switch (pos, exercise) {
    | (Programming(pos), Programming(exercise)) =>
      Programming(
        Programming.switch_editor(~pos, instructor_mode, ~exercise),
      )
    | (Proof(pos), Proof(exercise)) =>
      Proof(Proof.switch_editor(~pos, instructor_mode, ~exercise))
    | _ => failwith("Exercise(switch_editor): impossible")
    };

  let transition: transitionary_spec => spec =
    fun
    | Programming(spec) => Programming(Programming.transition(spec))
    | Proof(spec) => Proof(Proof.transition(spec));

  let switch_derivation_rule = (~pos: pos, ~new_rule, ~exercise: state) =>
    switch (pos, exercise) {
    | (Proof(pos), Proof(exercise)) =>
      Proof(Proof.switch_derivation_rule(~pos, ~new_rule, ~exercise))
    | _ => exercise
    };

  let set_instructor_mode = (state: state, new_mode: bool) =>
    switch (state) {
    | Programming(state) =>
      Programming(Programming.set_instructor_mode(state, new_mode))
    | Proof(state) => Proof(Proof.set_instructor_mode(state, new_mode))
    };

  let state_of_spec = (spec: spec, ~instructor_mode: bool): state =>
    switch (spec) {
    | Programming(spec) =>
      Programming(Programming.state_of_spec(spec, ~instructor_mode))
    | Proof(spec) => Proof(Proof.state_of_spec(spec, ~instructor_mode))
    };

  let persistent_state_of_state_result_modifiers =
      (f: 'a => pos, (pos, zippers): ('a, list(('a, PersistentZipper.t))))
      : persistent_state => (
    f(pos),
    List.map(((p, z)) => (f(p), z), zippers),
  );

  let persistent_state_of_state =
      (state: state, ~instructor_mode: bool): persistent_state =>
    switch (state) {
    | Programming(state) =>
      Programming.persistent_state_of_state(state, ~instructor_mode)
      |> persistent_state_of_state_result_modifiers((ed) =>
           (Programming(ed): pos)
         )
    | Proof(state) =>
      Proof.persistent_state_of_state(state, ~instructor_mode)
      |> persistent_state_of_state_result_modifiers((ed) => (Proof(ed): pos))
    };

  let unpersist_state =
      (
        (pos, positioned_zippers): persistent_state,
        ~spec: spec,
        ~instructor_mode: bool,
      )
      : state =>
    switch (pos, spec) {
    | (Programming(pos), Programming(spec)) =>
      let positioned_zippers =
        List.map(
          ((p: pos, z)) =>
            switch (p) {
            | Programming(p) => (p, z)
            | _ => failwith("Exercise(unpersist_state): impossible")
            },
          positioned_zippers,
        );
      Programming(
        Programming.unpersist_state(
          (pos, positioned_zippers),
          ~spec,
          ~instructor_mode,
        ),
      );
    | (Proof(pos), Proof(spec)) =>
      let positioned_zippers =
        List.map(
          ((p: pos, z)) =>
            switch (p) {
            | Proof(p) => (p, z)
            | _ => failwith("Exercise(unpersist_state): impossible")
            },
          positioned_zippers,
        );
      Proof(
        Proof.unpersist_state(
          (pos, positioned_zippers),
          ~spec,
          ~instructor_mode,
        ),
      );
    | _ => failwith("Exercise(unpersist_state): impossible")
    };

  // # Stitching

  type stitched('a) =
    | Programming(Programming.stitched('a))
    | Proof(Proof.stitched('a));

  // type stitched_statics = stitched(StaticsItem.t);

  let key_for_statics = (state: state): string =>
    switch (state) {
    | Programming(state) => Programming.key_for_statics(state)
    | Proof(state) => Proof.key_for_statics(state)
    };

  let spliced_elabs =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, DHExp.t)) =>
    switch (state) {
    | Programming(state) => Programming.spliced_elabs(settings, state)
    | Proof(state) => Proof.spliced_elabs(settings, state)
    };

  let mk_statics =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, StaticsItem.t)) =>
    switch (state) {
    | Programming(state) => Programming.mk_statics(settings, state)
    | Proof(state) => Proof.mk_statics(settings, state)
    };

  let stitch_dynamic =
      (
        settings: CoreSettings.t,
        state: state,
        results: option(ModelResults.t),
      )
      : stitched(DynamicsItem.t) =>
    switch (state) {
    | Programming(state) =>
      Programming.stitch_dynamic(settings, state, results)
      |> (d => Programming(d))
    | Proof(state) =>
      Proof.stitch_dynamic(settings, state, results) |> (d => Proof(d))
    };
  let stitch_dynamic = Core.Memo.general(stitch_dynamic);

  // Module Export

  // TODO: to be refactored
  let editor_pp = (fmt, editor: Editor.t) => {
    let zipper = editor.state.zipper;
    let serialization = Zipper.show(zipper);
    // let string_literal = "\"" ++ String.escaped(serialization) ++ "\"";
    Format.pp_print_string(fmt, serialization);
  };

  let export_module = (module_name, eds: eds) => {
    let prefix =
      "let prompt = "
      ++ module_name
      ++ "_prompt.prompt\n"
      ++ "let exercise: Exercise.spec = ";
    let record = show_p(editor_pp, eds);
    prefix ++ record ++ "\n";
  };

  let transitionary_editor_pp = (fmt, editor: Editor.t) => {
    let zipper = editor.state.zipper;
    let code = Printer.to_string_basic(zipper);
    Format.pp_print_string(fmt, "\"" ++ String.escaped(code) ++ "\"");
  };

  let export_transitionary_module = (module_name, eds: eds) => {
    let prefix =
      "let prompt = "
      ++ module_name
      ++ "_prompt.prompt\n"
      ++ "let exercise: Exercise.spec = Exercise.transition(";
    let record = show_p(transitionary_editor_pp, eds);
    prefix ++ record ++ ")\n";
  };

  let export_grading_module = (module_name, eds: eds) => {
    let header = output_header_grading(module_name);
    let prefix = "let exercise: Exercise.spec = ";
    let record = show_p(editor_pp, eds);
    header ++ prefix ++ record ++ "\n";
  };

  // From Store

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
};
