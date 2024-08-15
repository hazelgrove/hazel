open Util;
open Haz3lcore;

module type ExerciseEnv = {
  type node;
  let default: node;
  let output_header: string => string;
};

let output_header_grading = _module_name =>
  "module Exercise = GradePrelude.Exercise\n" ++ "let prompt = ()\n";

module F = (ExerciseEnv: ExerciseEnv) => {
  module Programming = ProgrammingCore;
  module Proof = ProofCore;
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
    | Programming(Programming.model('code))
    | Proof(Proof.model('code));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type pos =
    | Programming(Programming.pos)
    | Proof(Proof.pos);

  module ModelUtil = {
    let map = (f: 'a => 'b, m: model('a)): model('b) =>
      switch (m) {
      | Programming(m) => Programming(Programming.ModelUtil.map(f, m))
      | Proof(m) => Proof(Proof.ModelUtil.map(f, m))
      };

    let mapi = (f: (pos, 'a) => 'b, m: model('a)): model('b) =>
      switch (m) {
      | Programming(m) =>
        Programming(
          Programming.ModelUtil.mapi(pos => Programming(pos) |> f, m),
        )
      | Proof(m) => Proof(Proof.ModelUtil.mapi(pos => Proof(pos) |> f, m))
      };

    let nth = (m: model('a), pos: pos): 'a =>
      switch (pos, m) {
      | (Programming(pos), Programming(m)) =>
        Programming.ModelUtil.nth(m, pos)
      | (Proof(pos), Proof(m)) => Proof.ModelUtil.nth(m, pos)
      | _ => failwith("Exercise(nth): mismatch")
      };

    let map_nth = (f: 'a => 'a, m: model('a), pos: pos): model('a) => {
      switch (pos, m) {
      | (Programming(pos), Programming(m)) =>
        Programming(Programming.ModelUtil.map_nth(f, m, pos))
      | (Proof(pos), Proof(m)) => Proof(Proof.ModelUtil.map_nth(f, m, pos))
      | _ => failwith("Exercise(map_nth): position mismatch")
      };
    };

    let flatten = (m: model('a)) =>
      switch (m) {
      | Programming(m) => Programming.ModelUtil.flatten(m)
      | Proof(m) => Proof.ModelUtil.flatten(m)
      };

    let switch_editor = (pos: pos, instructor_mode: bool): bool =>
      switch (pos) {
      | Programming(pos) =>
        Programming.ModelUtil.switch_editor(pos, instructor_mode)
      | Proof(pos) => Proof.ModelUtil.switch_editor(pos, instructor_mode)
      };

    let readonly_in = (pos: pos, instructor_mode: bool): bool =>
      switch (pos) {
      | Programming(pos) =>
        Programming.ModelUtil.readonly_in(pos, instructor_mode)
      | Proof(pos) => Proof.ModelUtil.readonly_in(pos, instructor_mode)
      };

    let visible_in = (pos: pos, instructor_mode: bool): bool =>
      switch (pos) {
      | Programming(pos) =>
        Programming.ModelUtil.visible_in(pos, instructor_mode)
      | Proof(pos) => Proof.ModelUtil.visible_in(pos, instructor_mode)
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

  let editor_of_state = ({pos, model, _}: state): Editor.t => {
    prerr_endline(model |> show_model(Editor.pp));
    print_endline("pos: " ++ show_pos(pos));
    ModelUtil.nth(model, pos);
  };

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

  let set_instructor_mode = (exercise: state, ~new_mode: bool) => {
    ...exercise,
    model:
      ModelUtil.mapi(
        (pos, editor) =>
          Editor.set_read_only(editor, ModelUtil.readonly_in(pos, new_mode)),
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

  type stitched('a) =
    | Programming(Programming.stitched('a))
    | Proof(Proof.stitched('a));

  module StitchUtil = {
    let stitch = (stitch2: ('a, 'a) => 'a, m: model('a)): stitched('a) =>
      switch (m) {
      | Programming(m) =>
        Programming(Programming.StitchUtil.stitch(stitch2, m))
      | Proof(m) => Proof(Proof.StitchUtil.stitch(stitch2, m))
      };
    let map = (f: 'a => 'b, s: stitched('a)): stitched('b) =>
      switch (s) {
      | Programming(s) => Programming(Programming.StitchUtil.map(f, s))
      | Proof(s) => Proof(Proof.StitchUtil.map(f, s))
      };

    let mapi = (f: (pos, 'a) => 'b, s: stitched('a)): stitched('b) =>
      switch (s) {
      | Programming(s) =>
        Programming(
          Programming.StitchUtil.mapi(
            (pos, x) => f(Programming(pos), x),
            s,
          ),
        )
      | Proof(s) =>
        Proof(Proof.StitchUtil.mapi((pos, x) => f(Proof(pos), x), s))
      };

    let nth = (s: stitched('a), pos: pos): 'a =>
      switch (pos, s) {
      | (Programming(pos), Programming(s)) =>
        Programming.StitchUtil.nth(s, pos)
      | (Proof(pos), Proof(s)) => Proof.StitchUtil.nth(s, pos)
      | _ => failwith("Exercise(StitchUtil.nth): mismatch")
      };

    let flatten = (s: stitched('a)) =>
      switch (s) {
      | Programming(s) => Programming.StitchUtil.flatten(s)
      | Proof(s) => Proof.StitchUtil.flatten(s)
      };

    let key = (pos: pos): string =>
      switch (pos) {
      | Programming(pos) => Programming.StitchUtil.key(pos)
      | Proof(pos) => Proof.StitchUtil.key(pos)
      };

    let fill = (m: model('a), init: pos => 'b): stitched('b) =>
      switch (m) {
      | Programming(m) =>
        Programming(
          Programming.StitchUtil.fill(m, pos => Programming(pos) |> init),
        )
      | Proof(m) =>
        Proof(Proof.StitchUtil.fill(m, pos => Proof(pos) |> init))
      };
  };

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
