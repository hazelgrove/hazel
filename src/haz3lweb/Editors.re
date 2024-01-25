open Sexplib.Std;
open Haz3lcore;
open Util;

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
  | Documentation(string, list((string, ScratchSlide.state)))
  | Exercises(int, list(Exercise.spec), Exercise.state);

let get_editor = (editors: t): Editor.t =>
  switch (editors) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(n, slides) =>
    assert(n < List.length(slides));
    List.nth(slides, n);
  | Documentation(name, slides) =>
    assert(List.mem_assoc(name, slides));
    List.assoc(name, slides);
  | Exercises(_, _, exercise) => Exercise.editor_of_state(exercise)
  };

let put_editor = (ed: Editor.t, eds: t): t =>
  switch (eds) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(n, slides) =>
    assert(n < List.length(slides));
    Scratch(n, Util.ListUtil.put_nth(n, ed, slides));
  | Documentation(name, slides) =>
    assert(List.mem_assoc(name, slides));
    Documentation(name, slides |> ListUtil.update_assoc((name, ed)));
  | Exercises(n, specs, exercise) =>
    Exercises(n, specs, Exercise.put_editor(exercise, ed))
  };

let active_zipper = (editors: t): Zipper.t =>
  get_editor(editors).state.zipper;

let get_ctx_init = (~settings as _: Settings.t, editors: t): Ctx.t =>
  switch (editors) {
  | Scratch(_)
  | DebugLoad
  | Exercises(_)
  | Documentation(_) => Builtins.ctx_init
  };

let get_env_init = (~settings as _: Settings.t, editors: t): Environment.t =>
  switch (editors) {
  | Scratch(_)
  | DebugLoad
  | Exercises(_)
  | Documentation(_) => Builtins.env_init
  };

/* Each mode (e.g. Scratch, School) requires
   elaborating on some number of expressions
   that are spliced together from the editors
   in the mode. Each elaborated expression
   is given a key for later lookup by the mode.

   Used in the Update module */
let get_spliced_elabs =
    (~settings: Settings.t, editors: t): list((ModelResults.key, DHExp.t)) => {
  settings.core.dynamics
    ? {
      let ctx_init = get_ctx_init(~settings, editors);
      switch (editors) {
      | DebugLoad => []
      | Scratch(idx, slides) =>
        let current_slide = List.nth(slides, idx);
        let (key, d) =
          ScratchSlide.spliced_elab(
            ~settings,
            ~ctx_init,
            string_of_int(idx),
            current_slide,
          );
        [(key, d)];
      | Documentation(name, slides) =>
        let current_slide = List.assoc(name, slides);
        let (key, d) =
          ScratchSlide.spliced_elab(
            ~settings,
            ~ctx_init,
            name,
            current_slide,
          );
        [(key, d)];
      | Exercises(_, _, exercise) =>
        Exercise.spliced_elabs(settings.core, exercise)
      };
    }
    : [];
};

let set_instructor_mode = (editors: t, instructor_mode: bool): t =>
  switch (editors) {
  | DebugLoad => failwith("no editors in debug load mode")
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
  | DebugLoad => failwith("impossible")
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
  | DebugLoad
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
  | DebugLoad
  | Scratch(_)
  | Exercises(_) => None
  | Documentation(cur, slides)
      when !List.mem_assoc(name, slides) || cur == name =>
    None
  | Documentation(_, slides) => Some(Documentation(name, slides))
  };
