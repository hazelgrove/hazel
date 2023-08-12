open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch = (int, list(ScratchSlide.state));

[@deriving (show({with_path: false}), sexp, yojson)]
type examples = (string, list((string, ScratchSlide.state)));

[@deriving (show({with_path: false}), sexp, yojson)]
type exercises = (int, list(Exercise.spec), Exercise.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | DebugLoad
  | Scratch(int, list(ScratchSlide.state))
  | Examples(string, list((string, ScratchSlide.state)))
  | Exercise(int, list(Exercise.spec), Exercise.state);

let get_editor = (editors: t): Editor.t =>
  switch (editors) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(n, slides) =>
    assert(n < List.length(slides));
    List.nth(slides, n);
  | Examples(name, slides) =>
    assert(List.mem_assoc(name, slides));
    List.assoc(name, slides);
  | Exercise(_, _, exercise) => Exercise.editor_of_state(exercise)
  };

let put_editor = (ed: Editor.t, eds: t): t =>
  switch (eds) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(n, slides) =>
    assert(n < List.length(slides));
    Scratch(n, Util.ListUtil.put_nth(n, ed, slides));
  | Examples(name, slides) =>
    assert(List.mem_assoc(name, slides));
    Examples(
      name,
      slides |> List.remove_assoc(name) |> List.cons((name, ed)),
    );
  | Exercise(n, specs, exercise) =>
    Exercise(n, specs, Exercise.put_editor(exercise, ed))
  };

let active_zipper = (editors: t): Zipper.t =>
  get_editor(editors).state.zipper;

let export_ctx = (~settings: Settings.t, init_ctx: Ctx.t, ed: Editor.t): Ctx.t => {
  let info =
    ed.state.zipper
    |> MakeTerm.from_zip_for_sem
    |> fst
    |> Interface.Statics.mk_map_ctx(settings.core, init_ctx)
    |> Id.Map.find_opt(Hyper.export_id);
  switch (info) {
  | None =>
    /* print_endline(
         "WARN: export_ctx: NOT found id= " ++ string_of_int(Hyper.export_id),
       );*/
    init_ctx
  | Some(info) => Info.ctx_of(info)
  };
};

let export_env =
    (
      ~settings: Settings.t,
      ctx_init: Ctx.t,
      env_init: Environment.t,
      ed: Editor.t,
    ) => {
  let tests =
    Interface.eval_editor(~settings=settings.core, ~env_init, ~ctx_init, ed)
    |> ProgramResult.get_state
    |> EvaluatorState.get_tests
    |> TestMap.lookup(Hyper.export_id);
  switch (tests) {
  | None
  | Some([]) =>
    /*print_endline(
        "WARN: export_env: NOT found id= " ++ string_of_int(Hyper.export_id),
      );*/
    env_init
  | Some([(_, _, env), ..._]) => env
  };
};

let deps = (fn: ('a, 'b) => 'a, acc_0: 'a, slides, idx) => {
  let get = idx => List.nth(slides, idx);
  let acc_1 = 1 |> get |> fn(acc_0);
  let acc_2 = 2 |> get |> fn(acc_1);
  let acc_3 = 3 |> get |> fn(acc_2);
  let acc_4 = 4 |> get |> fn(acc_3);
  let acc_5 = 5 |> get |> fn(acc_4);
  let acc_6 = 6 |> get |> fn(acc_5);
  switch (idx) {
  | 0 => acc_0
  | 1 => acc_0
  | 2 => acc_1
  | 3 => acc_2
  | 4 => acc_3
  | 5 => acc_4
  | 6 => acc_5
  | _ => acc_6
  };
};

let get_ctx_init_slides = (~settings: Settings.t, editors, idx) =>
  settings.core.statics
    ? deps(export_ctx(~settings), Builtins.ctx_init, editors, idx)
    : Builtins.ctx_init;

let get_env_init_slides = (~settings: Settings.t, ctx_init, editors, idx) =>
  settings.core.dynamics
    ? deps(export_env(~settings, ctx_init), Builtins.env_init, editors, idx)
    : Builtins.env_init;

let get_ctx_init = (~settings: Settings.t, editors: t): Ctx.t =>
  switch (editors) {
  | Scratch(idx, slides) when settings.core.statics =>
    get_ctx_init_slides(~settings, slides, idx)
  | Scratch(_)
  | DebugLoad
  | Exercise(_)
  | Examples(_) => Builtins.ctx_init
  };

let get_env_init = (~settings: Settings.t, editors: t): Environment.t =>
  switch (editors) {
  | Scratch(idx, slides) when settings.core.dynamics =>
    get_env_init_slides(
      ~settings,
      get_ctx_init(~settings, editors),
      slides,
      idx,
    )
  | Scratch(_)
  | DebugLoad
  | Exercise(_)
  | Examples(_) => Builtins.env_init
  };

let get_spliced_elabs =
    (~settings: Settings.t, editors: t)
    : list((ModelResults.key, DHExp.t, Environment.t)) => {
  settings.core.dynamics
    ? {
      let ctx_init = get_ctx_init(~settings, editors);
      let env_init = get_env_init(~settings, editors);
      switch (editors) {
      | DebugLoad => []
      | Scratch(idx, slides) =>
        let current_slide = List.nth(slides, idx);
        let (key, d) =
          ScratchSlide.spliced_elab(~settings, ~ctx_init, current_slide);
        [(key, d, env_init)];
      | Examples(name, slides) =>
        let current_slide = List.assoc(name, slides);
        let (key, d) =
          ScratchSlide.spliced_elab(~settings, ~ctx_init, current_slide);
        [(key, d, env_init)];
      | Exercise(_, _, exercise) =>
        Exercise.spliced_elabs(~settings=settings.core, exercise)
      };
    }
    : [];
};

let set_instructor_mode = (editors: t, instructor_mode: bool): t =>
  switch (editors) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(_)
  | Examples(_) => editors
  | Exercise(n, specs, exercise) =>
    Exercise(
      n,
      specs,
      Exercise.set_instructor_mode(exercise, instructor_mode),
    )
  };

let reset_nth_slide = (n, slides) => {
  let data = List.nth(Init.startup.scratch |> snd, n);
  let init_nth = ScratchSlide.unpersist(data);
  Util.ListUtil.put_nth(n, init_nth, slides);
};

let reset_named_slide = (name, slides) => {
  let data = List.assoc(name, Init.startup.examples |> snd);
  let init_name = ScratchSlide.unpersist(data);
  slides |> List.remove_assoc(name) |> List.cons((name, init_name));
};

let reset_current = (editors: t, ~instructor_mode: bool): t =>
  switch (editors) {
  | DebugLoad => failwith("impossible")
  | Scratch(n, slides) => Scratch(n, reset_nth_slide(n, slides))
  | Examples(name, slides) =>
    Examples(name, reset_named_slide(name, slides))
  | Exercise(n, specs, _) =>
    Exercise(
      n,
      specs,
      List.nth(specs, n) |> Exercise.state_of_spec(~instructor_mode),
    )
  };

let import_current = (editors: t, data: option(string)): t =>
  switch (editors) {
  | DebugLoad
  | Examples(_)
  | Exercise(_) => failwith("impossible")
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
  | DebugLoad
  | Scratch(_)
  | Exercise(_) => None
  | Examples(cur, slides) when !List.mem_assoc(name, slides) || cur == name =>
    None
  | Examples(_, slides) => Some(Examples(name, slides))
  };
