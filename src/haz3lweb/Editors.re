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

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | DebugLoad
  | Scratch
  | Examples
  | Exercise;

let mode_of_string = (s: string): mode =>
  switch (s) {
  | "Scratch" => Scratch
  | "Examples" => Examples
  | "Exercise" => Exercise
  | _ => Scratch
  };

let get_editor_and_id = (editors: t): (Id.t, Editor.t) =>
  switch (editors) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(n, slides) =>
    assert(n < List.length(slides));
    let slide = List.nth(slides, n);
    let id = ScratchSlide.id_of_state(slide);
    let ed = ScratchSlide.editor_of_state(slide);
    (id, ed);
  | Examples(name, slides) =>
    assert(List.mem_assoc(name, slides));
    let slide = List.assoc(name, slides);
    let id = ScratchSlide.id_of_state(slide);
    let ed = ScratchSlide.editor_of_state(slide);
    (id, ed);
  | Exercise(_, _, exercise) =>
    let id = Exercise.id_of_state(exercise);
    let ed = Exercise.editor_of_state(exercise);
    (id, ed);
  };

let get_editor = (editors: t): Editor.t => snd(get_editor_and_id(editors));

let put_editor_and_id = (id: Id.t, ed: Editor.t, eds: t): t =>
  switch (eds) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(n, slides) =>
    assert(n < List.length(slides));
    let slide = List.nth(slides, n);
    Scratch(
      n,
      Util.ListUtil.put_nth(
        n,
        ScratchSlide.put_editor_and_id(slide, id, ed),
        slides,
      ),
    );
  | Examples(name, slides) =>
    assert(List.mem_assoc(name, slides));
    let slide = List.assoc(name, slides);
    Examples(
      name,
      slides
      |> List.remove_assoc(name)
      |> List.cons((name, ScratchSlide.put_editor_and_id(slide, id, ed))),
    );
  | Exercise(n, specs, exercise) =>
    Exercise(n, specs, Exercise.put_editor_and_id(exercise, id, ed))
  };

let active_zipper = (editors: t): Zipper.t =>
  get_editor(editors).state.zipper;

let export_ctx = (_idx: int, init_ctx: Ctx.t, ed: Editor.t): Ctx.t => {
  let (term, _) = MakeTerm.from_zip_for_sem(ed.state.zipper);
  let info_map = Statics.mk_map_ctx(init_ctx, term);

  switch (Id.Map.find_opt(Hyper.export_id, info_map)) {
  | None =>
    print_endline(
      "export_ctx: NOT found info_map, id= " ++ string_of_int(Hyper.export_id),
    );
    init_ctx;
  | Some(info) =>
    print_endline(
      "export_ctx: FOUND in info_map, id= " ++ string_of_int(Hyper.export_id),
    );
    Info.ctx_of(info);
  };
};

let export_env =
    (ctx_init: Ctx.t, idx: int, env_init: Environment.t, ed: Editor.t) => {
  let tests =
    Interface.eval_editor(~env_init, ~ctx_init, ed)
    |> ProgramResult.get_state
    |> EvaluatorState.get_tests
    |> TestMap.lookup(Hyper.export_id + idx);
  switch (tests) {
  | Some([(_, _, env), ..._]) => env
  | Some([]) =>
    print_endline(
      "WARNING: export_env: testmap lookup returned empty list for id: "
      ++ string_of_int(Hyper.export_id + idx),
    );
    Environment.empty;
  | None =>
    print_endline(
      "WARNING: export_env: testmap lookup failed for id: "
      ++ string_of_int(Hyper.export_id + idx),
    );
    Environment.empty;
  };
};

let deps = (fn: (int, 'a, 'b) => 'a, acc_0: 'a, slides, idx) => {
  let get = idx => List.nth(slides, idx) |> snd;
  let acc_1 = 1 |> get |> fn(0, acc_0);
  let acc_2 = 2 |> get |> fn(1, acc_1);
  let acc_3 = 3 |> get |> fn(2, acc_2);
  let acc_4 = 4 |> get |> fn(3, acc_3);
  let acc_5 = 5 |> get |> fn(4, acc_4);
  let acc_6 = 6 |> get |> fn(5, acc_5);
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

let get_ctx_init_slides =
  deps(export_ctx, Builtins.ctx(Builtins.Pervasives.builtins));
let get_env_init_slides = ctx_init =>
  deps(export_env(ctx_init), Environment.empty);

let get_ctx_init = (editors: t): Ctx.t =>
  switch (editors) {
  | Scratch(idx, slides) => get_ctx_init_slides(slides, idx)
  | DebugLoad
  | Exercise(_)
  | Examples(_) => Builtins.ctx(Builtins.Pervasives.builtins)
  };

let get_env_init = (editors: t): Environment.t =>
  switch (editors) {
  | Scratch(idx, slides) =>
    get_env_init_slides(get_ctx_init(editors), slides, idx)
  | DebugLoad
  | Exercise(_)
  | Examples(_) => Environment.empty
  };

//TODO(andrew): cleanup
[@deriving (show({with_path: false}), sexp, yojson)]
type blah = list((Var.t, DHExp.t));
let get_spliced_elabs =
    (editors: t): list((ModelResults.key, DHExp.t, Environment.t)) => {
  let ctx_init = get_ctx_init(editors);
  switch (editors) {
  | DebugLoad => []
  | Scratch(idx, slides) =>
    //print_endline("get_spliced_elabs: idx= " ++ string_of_int(idx));
    let current_slide = List.nth(slides, idx);
    //print_endline("ctx=" ++ Ctx.show(ctx_init));
    let env_init = get_env_init_slides(ctx_init, slides, idx);
    //print_endline("env=" ++ Environment.show(env_init));
    let (key, d) = ScratchSlide.spliced_elab(~ctx_init, current_slide);
    [(key, d, env_init)];
  | Examples(name, slides) =>
    let slide = List.assoc(name, slides);
    let (key, d) = ScratchSlide.spliced_elab(~ctx_init, slide);
    [(key, d, Environment.empty)];
  | Exercise(_, _, exercise) => Exercise.spliced_elabs(exercise)
  };
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

let reset_current = (editors: t, ~instructor_mode: bool): t =>
  switch (editors) {
  | DebugLoad => failwith("impossible")
  | Scratch(n, slides) =>
    let slides =
      Util.ListUtil.put_nth(n, ScratchSlidesInit.init_nth(n), slides);
    Scratch(n, slides);
  | Examples(name, slides) =>
    let slides =
      slides
      |> List.remove_assoc(name)
      |> List.cons((name, Examples.init_name(name)));
    Examples(name, slides);
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
