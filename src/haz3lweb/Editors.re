open Sexplib.Std;
open Haz3lcore;
open Util;
open OptUtil.Syntax;

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cell =
    | MainEditor
    | Result(ModelResult.selection);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Scratch(cell)
    | Exercises(Exercise.pos, cell);
};

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cell =
    | MainEditor(Action.t)
    | Result(ModelResult.action);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Scratch(cell)
    | Documentation(cell)
    | Exercises(Exercise.pos, cell);

  module Failure = Action.Failure;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Scratch(int, list(ScratchSlide.state))
  | Documentation(string, list((string, ScratchSlide.state)))
  | Exercises(int, list(Exercise.spec), Exercise.state);

let update_scratch_state =
    (f: ScratchSlide.state => ScratchSlide.state, editors: t): t =>
  switch (editors) {
  | Scratch(n, slides) => Scratch(n, ListUtil.map_nth(n, f, slides))
  | Documentation(str, slides) =>
    Documentation(
      str,
      ListUtil.update_assoc((str, f(List.assoc(str, slides))), slides),
    )
  // TODO[Matt]: update exercises
  | Exercises(_) => editors
  };

let get_ctx_init = (~settings as _: Settings.t, editors: t): Ctx.t =>
  switch (editors) {
  | Scratch(_)
  | Exercises(_)
  | Documentation(_) => Builtins.ctx_init
  };

let get_env_init = (~settings as _: Settings.t, editors: t): Environment.t =>
  switch (editors) {
  | Scratch(_)
  | Exercises(_)
  | Documentation(_) => Builtins.env_init
  };

let mk_statics = (~settings: Settings.t, editors: t): CachedStatics.t => {
  let ctx_init = get_ctx_init(~settings, editors);
  switch (editors) {
  | _ when !settings.core.statics => CachedStatics.mk([])
  | Scratch(idx, slides) =>
    let key = ScratchSlide.scratch_key(string_of_int(idx));
    let editor = List.nth(slides, idx);
    [(key, ScratchSlide.mk_statics(~settings, editor, ctx_init))]
    |> CachedStatics.mk;
  | Documentation(name, slides) =>
    let key = ScratchSlide.scratch_key(name);
    let editor = List.assoc(name, slides);
    [(key, ScratchSlide.mk_statics(~settings, editor, ctx_init))]
    |> CachedStatics.mk;
  | Exercises(_, _, exercise) =>
    Exercise.mk_statics(settings.core, exercise) |> CachedStatics.mk
  };
};

let lookup_statics =
    (~settings: Settings.t, ~selection: Selection.t, ~statics, editors: t)
    : CachedStatics.statics =>
  switch (editors, selection) {
  | _ when !settings.core.statics => CachedStatics.empty_statics
  | (Scratch(idx, _), Scratch(_)) =>
    let key = ScratchSlide.scratch_key(string_of_int(idx));
    CachedStatics.lookup(statics, key);
  | (Documentation(name, _), Scratch(_)) =>
    let key = ScratchSlide.scratch_key(name);
    CachedStatics.lookup(statics, key);
  | (Exercises(_), Exercises(pos, _)) =>
    let key = Exercise.key_for_statics(pos);
    CachedStatics.lookup(statics, key);
  | (Exercises(_), _)
  | (Documentation(_), _)
  | (Scratch(_), _) => CachedStatics.empty_statics
  };

/* Each mode (e.g. Scratch, School) requires
   elaborating on some number of expressions
   that are spliced together from the editors
   in the mode. Each elaborated expression
   is given a key for later lookup by the mode.

   Used in the Update module */
let get_spliced_elabs =
    (~settings: Settings.t, statics, editors: t)
    : list((ModelResults.key, Elaborator.Elaboration.t)) =>
  switch (editors) {
  | Scratch(idx, _) =>
    let key = ScratchSlide.scratch_key(idx |> string_of_int);
    let CachedStatics.{term, info_map, _} =
      lookup_statics(
        ~settings,
        ~selection=Scratch(MainEditor),
        ~statics,
        editors,
      );
    let d = Interface.elaborate(~settings=settings.core, info_map, term);
    [(key, {d: d})];
  | Documentation(name, _) =>
    let key = ScratchSlide.scratch_key(name);
    let CachedStatics.{term, info_map, _} =
      lookup_statics(
        ~settings,
        ~selection=Scratch(MainEditor),
        ~statics,
        editors,
      );
    let d = Interface.elaborate(~settings=settings.core, info_map, term);
    [(key, {d: d})];
  | Exercises(_, _, exercise) =>
    Exercise.spliced_elabs(settings.core, exercise)
  };

let set_instructor_mode = (editors: t, instructor_mode: bool): t =>
  switch (editors) {
  | Scratch(_)
  | Documentation(_) => editors
  | Exercises(n, specs, exercise) =>
    Exercises(
      n,
      specs,
      Exercise.set_instructor_mode(exercise, instructor_mode),
    )
  };

let reset_nth_slide = (n, slides) => {
  let (_, init_editors, _) = Init.startup.scratch;
  let data = List.nth(init_editors, n);
  let init_nth = ScratchSlide.unpersist(data);
  Util.ListUtil.put_nth(n, init_nth, slides);
};

let reset_named_slide = (name, slides) => {
  let (_, init_editors, _) = Init.startup.documentation;
  let data = List.assoc(name, init_editors);
  let init_name = ScratchSlide.unpersist(data);
  slides |> List.remove_assoc(name) |> List.cons((name, init_name));
};

let reset_current = (editors: t, ~instructor_mode: bool): t =>
  switch (editors) {
  | Scratch(n, slides) => Scratch(n, reset_nth_slide(n, slides))
  | Documentation(name, slides) =>
    Documentation(name, reset_named_slide(name, slides))
  | Exercises(n, specs, _) =>
    Exercises(
      n,
      specs,
      List.nth(specs, n) |> Exercise.state_of_spec(~instructor_mode),
    )
  };

let import_current = (editors: t, data: option(string)): t =>
  switch (editors) {
  | Documentation(_)
  | Exercises(_) => failwith("impossible")
  | Scratch(idx, slides) =>
    switch (data) {
    | None => editors
    | Some(data) =>
      let state = ScratchSlide.import(data);
      let slides = Util.ListUtil.put_nth(idx, state, slides);
      Scratch(idx, slides);
    }
  };

let switch_example_slide = (editors: t, name: string): option(t) =>
  switch (editors) {
  | Scratch(_)
  | Exercises(_) => None
  | Documentation(cur, slides)
      when !List.mem_assoc(name, slides) || cur == name =>
    None
  | Documentation(_, slides) => Some(Documentation(name, slides))
  };

let get_selected_editor =
    (~selection: Selection.t, editors: t, model_results: ModelResults.t) =>
  switch (editors, selection) {
  | (Documentation(name, slides), Scratch(MainEditor)) =>
    List.assoc_opt(name, slides)
  | (Documentation(n, _), Scratch(Result(selection))) =>
    let key = ScratchSlide.scratch_key(n);
    let* result = ModelResults.lookup(model_results, key);
    ModelResult.get_selected_editor(~selection, result);
  | (Documentation(_), _) => None
  | (Scratch(idx, slides), Scratch(MainEditor)) =>
    List.nth_opt(slides, idx)
  | (Scratch(n, _), Scratch(Result(selection))) =>
    let key = ScratchSlide.scratch_key(string_of_int(n));
    let* result = ModelResults.lookup(model_results, key);
    ModelResult.get_selected_editor(~selection, result);
  | (Scratch(_), _) => None
  | (Exercises(_, _, exercise), Exercises(selection, MainEditor)) =>
    Some(Exercise.main_editor_of_state(~selection, exercise))
  | (Exercises(_), Exercises(pos, Result(selection))) =>
    let* result =
      ModelResults.lookup(model_results, Exercise.key_for_statics(pos));
    ModelResult.get_selected_editor(~selection, result);
  | (Exercises(_), _) => None
  };

let put_selected_editor =
    (
      ~selection: Selection.t,
      editors: t,
      model_results: ModelResults.t,
      editor: Editor.t,
    )
    : (t, ModelResults.t) =>
  switch (editors, selection) {
  | (Documentation(name, slides), Scratch(MainEditor)) =>
    let slides = ListUtil.update_assoc((name, editor), slides);
    (Documentation(name, slides), model_results);
  | (Documentation(n, _), Scratch(Result(selection))) =>
    let key = ScratchSlide.scratch_key(n);
    let results = {
      let+ result = ModelResults.lookup(model_results, key);
      let result =
        ModelResult.put_selected_editor(~selection, result, editor);
      ModelResults.put_result(key, result, model_results);
    };
    switch (results) {
    | Some(results) => (editors, results)
    | None => (editors, model_results)
    };
  | (Documentation(_), _) => (editors, model_results)
  | (Scratch(idx, slides), Scratch(MainEditor)) =>
    let slides = ListUtil.map_nth(idx, _ => editor, slides);
    (Scratch(idx, slides), model_results);
  | (Scratch(n, _), Scratch(Result(selection))) =>
    let key = ScratchSlide.scratch_key(string_of_int(n));
    let results = {
      let+ result = ModelResults.lookup(model_results, key);
      let result =
        ModelResult.put_selected_editor(~selection, result, editor);
      ModelResults.put_result(key, result, model_results);
    };
    switch (results) {
    | Some(results) => (editors, results)
    | None => (editors, model_results)
    };
  | (Scratch(_), _) => (editors, model_results)
  | (Exercises(n, spec, exercise), Exercises(pos, MainEditor)) =>
    let exercise = Exercise.put_main_editor(~selection=pos, exercise, editor);
    (Exercises(n, spec, exercise), model_results);
  | (Exercises(_), Exercises(pos, Result(selection))) =>
    let key = Exercise.key_for_statics(pos);
    let results = {
      let+ result = ModelResults.lookup(model_results, key);
      let result =
        ModelResult.put_selected_editor(~selection, result, editor);
      ModelResults.put_result(key, result, model_results);
    };
    switch (results) {
    | Some(results) => (editors, results)
    | None => (editors, model_results)
    };
  | (Exercises(_), _) => (editors, model_results)
  };

let update_selected_editor =
    (
      ~selection: option(Selection.t),
      f: Editor.t => Result.t(Editor.t, Action.Failure.t),
      editors: t,
      results: ModelResults.t,
    )
    : Result.t((t, ModelResults.t), Action.Failure.t) => {
  switch (selection) {
  | None => Ok((editors, results))
  | Some(selection) =>
    switch (get_selected_editor(~selection, editors, results)) {
    | None => Ok((editors, results))
    | Some(editor) =>
      switch (f(editor)) {
      | Ok(editor) =>
        let (editors, results) =
          put_selected_editor(~selection, editors, results, editor);
        Ok((editors, results));
      | Error(e) => Error(e)
      }
    }
  };
};

let get_cursor_info =
    (
      ~selection: option(Selection.t),
      ~settings,
      editors: t,
      model_results: ModelResults.t,
      statics: CachedStatics.t,
    ) => {
  let* selection = selection;
  let statics = lookup_statics(~settings, ~selection, ~statics, editors);
  let* editor = get_selected_editor(~selection, editors, model_results);
  Indicated.ci_of(editor.state.zipper, statics.info_map);
};
