open Util;
open Haz3lcore;

module type ExerciseEnv = {
  type node;
  let default: node;
  let output_header: string => string;
};

let output_header_grading = _module_name =>
  "module Exercise = GradePrelude.Exercise\n" ++ "let prompt = ()\n";

module type ExerciseCore = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model('a);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type pos;
  module ModelUtil: {
    let map: ('a => 'b, model('a)) => model('b);
    let mapi: ((pos, 'a) => 'b, model('a)) => model('b);
    let nth: (model('a), pos) => 'a;
    let map_nth: ('a => 'a, model('a), pos) => model('a);
    let flatten: model('a) => list('a);
    let switch_editor: (pos, bool) => bool;
    let readonly_in: (pos, bool) => bool;
    let visible_in: (pos, bool) => bool;
  };
  type stitched('a);
  module StitchUtil: {
    let stitch: (('a, 'a) => 'a, model('a)) => stitched('a);
    let map: ('a => 'b, stitched('a)) => stitched('b);
    let mapi: ((pos, 'a) => 'b, stitched('a)) => stitched('b);
    let nth: (stitched('a), pos) => 'a;
    let flatten: stitched('a) => list('a);
    let key: pos => string;
    let fill: (model('a), pos => 'b) => stitched('b);
  };
};

module F = (ExerciseEnv: ExerciseEnv, ExerciseCore: ExerciseCore) => {
  open ExerciseCore;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type header = {
    title: string,
    version: int,
    module_name: string,
    prompt:
      [@printer (fmt, _) => Format.pp_print_string(fmt, "prompt")] [@opaque] ExerciseEnv.node,
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

  let editor_of_serialization = (zipper: Zipper.t, ~settings: CoreSettings.t) =>
    Editor.init(zipper, ~settings);

  let eds_of_spec = (~settings: CoreSettings.t, {model, _}: spec) =>
    model |> ModelUtil.map(editor_of_serialization(~settings));

  let switch_editor = (~pos: pos, instructor_mode, ~state: state) =>
    if (ModelUtil.switch_editor(pos, instructor_mode)) {
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
      (
        ~pos: Util.Tree.pos,
        ~index: int,
        ~exercise: state,
        ~settings: CoreSettings.t,
      )
      : state =>
    switch (exercise.model) {
    | Proof(m) =>
      let init = "" |> zipper_of_code |> Editor.init(~settings) |> Fun.const;
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
    ModelUtil.visible_in(pos, instructor_mode);

  let state_of_spec =
      (spec: spec, ~instructor_mode: bool, ~settings: CoreSettings.t): state =>
    {...spec, model: eds_of_spec(~settings, spec)}
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
        ~settings: CoreSettings.t,
      )
      : state => {
    let lookup = (pos, default) =>
      if (visible_in(pos, ~instructor_mode)) {
        let persisted_zipper = List.assoc(pos, positioned_zippers);
        let zipper = PersistentZipper.unpersist(persisted_zipper);
        Editor.init(zipper, ~settings);
      } else {
        editor_of_serialization(default, ~settings);
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

  let key_of_state = key;

  let key_of = key;

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

  let stitch_term = ({model, _}: state): stitched(UExp.t) => {
    let term_of = (editor: Editor.t): UExp.t =>
      MakeTerm.from_zip_for_sem(editor.state.zipper).term;
    // let wrap_filter = (act: FilterAction.action, term: UExp.t): UExp.t =>
    //   Exp.{
    //     term:
    //       Exp.Filter(
    //         Filter({
    //           act: FilterAction.(act, One),
    //           pat: {
    //             term: Constructor("$e", Unknown(Internal) |> Typ.temp),
    //             copied: false,
    //             ids: [Id.mk()],
    //           },
    //         }),
    //         term,
    //       ),
    //     copied: false,
    //     ids: [Id.mk()],
    //   };
    StitchUtil.stitch(
      EditorUtil.append_exp,
      model |> ModelUtil.map(term_of),
    );
  };
  let stitch_term = Core.Memo.general(stitch_term);

  type stitched_statics = stitched(Editor.CachedStatics.t);

  /* Multiple stitchings are needed for each exercise
     (see comments in the stitched type above)

     Stitching is necessary to concatenate terms
     from different editors, which are then typechecked. */
  let stitch_static =
      (settings: CoreSettings.t, t: stitched(UExp.t)): stitched_statics => {
    let mk = (term: UExp.t): Editor.CachedStatics.t => {
      let info_map = Statics.mk(settings, Builtins.ctx_init, term);
      {term, error_ids: Statics.Map.error_ids(info_map), info_map};
    };
    // Prelude should be `instructor`
    // works as long as you don't shadow anything in the prelude
    StitchUtil.map(mk, t);
  };
  let stitch_static = Core.Memo.general(stitch_static);

  // let statics_of = (~settings, exercise: state): Editor.CachedStatics.t =>
  //   StitchUtil.nth(
  //     exercise |> stitch_term |> stitch_static(settings),
  //     exercise.pos,
  //   );

  let key_for_statics = (state: state): string => StitchUtil.key(state.pos);

  let spliced_elabs =
      (settings: CoreSettings.t, state: state)
      : list((ModelResults.key, Elaborator.Elaboration.t)) => {
    let elab = (s: Editor.CachedStatics.t): Elaborator.Elaboration.t => {
      d: Interface.elaborate(~settings, s.info_map, s.term),
    };
    // Note: prelude should be filtered
    stitch_static(settings, stitch_term(state))
    |> StitchUtil.mapi((pos, si) => (StitchUtil.key(pos), elab(si)))
    |> StitchUtil.flatten;
  };

  module DynamicsItem = {
    type t = {
      statics: Editor.CachedStatics.t,
      result: ModelResult.t,
    };
    let empty: t = {statics: Editor.CachedStatics.empty, result: NoElab};
    let statics_only = (statics: Editor.CachedStatics.t): t => {
      statics,
      result: NoElab,
    };
  };

  let statics_of_stiched_dynamics =
      (state: state, s: stitched(DynamicsItem.t)): Editor.CachedStatics.t =>
    StitchUtil.nth(s, state.pos).statics;

  // let mk_statics =
  //     (settings: CoreSettings.t, state: state)
  //     : list((ModelResults.key, Editor.CachedStatics.t)) =>
  //   stitch_static(settings, stitch_term(state))
  //   |> StitchUtil.mapi((pos, si) => (StitchUtil.key(pos), si))
  //   |> StitchUtil.flatten;

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
    |> StitchUtil.mapi((pos, statics: Editor.CachedStatics.t) =>
         DynamicsItem.{statics, result: result_of(StitchUtil.key(pos))}
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
      |> StitchUtil.map(statics => DynamicsItem.statics_only(statics));
    } else {
      StitchUtil.fill(state.model, DynamicsItem.empty |> Fun.const);
    };

  let stitch_dynamic = Core.Memo.general(stitch_dynamic);
};
