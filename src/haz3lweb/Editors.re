open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch = {
  idx: int,
  slides: list(ScratchSlide.state),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type examples = {
  current: string,
  slides: list((string, ScratchSlide.state)),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type exercise = {
  idx: int,
  specs: list(Exercise.spec),
  state: Exercise.state,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | DebugLoad
  | Scratch(scratch)
  | Examples(examples)
  | Exercise(exercise);

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | DebugLoad
  | Scratch
  | Examples
  | Exercise;

let default: t = Scratch({idx: 0, slides: [(1, Editor.empty(0))]});

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
  | Scratch({idx, slides}) =>
    assert(idx < List.length(slides));
    let slide = List.nth(slides, idx);
    let id = ScratchSlide.id_of_state(slide);
    let ed = ScratchSlide.editor_of_state(slide);
    (id, ed);
  | Examples({current, slides}) =>
    assert(List.mem_assoc(current, slides));
    let slide = List.assoc(current, slides);
    let id = ScratchSlide.id_of_state(slide);
    let ed = ScratchSlide.editor_of_state(slide);
    (id, ed);
  | Exercise({state, _}) =>
    let id = Exercise.id_of_state(state);
    let ed = Exercise.editor_of_state(state);
    (id, ed);
  };

let get_editor = (editors: t): Editor.t => snd(get_editor_and_id(editors));

let put_editor_and_id = (id: Id.t, ed: Editor.t, eds: t): t =>
  switch (eds) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch({idx, slides}) =>
    assert(idx < List.length(slides));
    let slide = List.nth(slides, idx);
    let slides =
      Util.ListUtil.put_nth(
        idx,
        ScratchSlide.put_editor_and_id(slide, id, ed),
        slides,
      );
    Scratch({idx, slides});
  | Examples({current, slides}) =>
    assert(List.mem_assoc(current, slides));
    let slide = List.assoc(current, slides);
    let slides =
      slides
      |> List.remove_assoc(current)
      |> List.cons((current, ScratchSlide.put_editor_and_id(slide, id, ed)));
    Examples({current, slides});
  | Exercise({idx, specs, state}) =>
    Exercise({idx, specs, state: Exercise.put_editor_and_id(state, id, ed)})
  };

let get_zipper = (editors: t): Zipper.t => get_editor(editors).state.zipper;

let get_spliced_elabs = (editors: t): list((ModelResults.key, DHExp.t)) => {
  switch (editors) {
  | DebugLoad => []
  | Scratch({idx, slides}) =>
    let slide = List.nth(slides, idx);
    ScratchSlide.spliced_elabs(slide);
  | Examples({current, slides}) =>
    let slide = List.assoc(current, slides);
    ScratchSlide.spliced_elabs(slide);
  | Exercise({state, _}) => Exercise.spliced_elabs(state)
  };
};

let set_instructor_mode = (editors: t, instructor_mode: bool): t =>
  switch (editors) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(_)
  | Examples(_) => editors
  | Exercise({idx, specs, state}) =>
    Exercise({
      idx,
      specs,
      state: Exercise.set_instructor_mode(state, instructor_mode),
    })
  };

let reset_current = (editors: t, ~instructor_mode: bool): t =>
  switch (editors) {
  | DebugLoad => failwith("impossible")
  | Scratch({idx, slides}) =>
    let slides =
      Util.ListUtil.put_nth(idx, ScratchSlidesInit.init_nth(idx), slides);
    Scratch({idx, slides});
  | Examples({current, slides}) =>
    let slides =
      slides
      |> List.remove_assoc(current)
      |> List.cons((current, Examples.init_name(current)));
    Examples({current, slides});
  | Exercise({idx, specs, _}) =>
    Exercise({
      idx,
      specs,
      state:
        List.nth(specs, idx) |> Exercise.state_of_spec(~instructor_mode),
    })
  };

let import_current = (editors: t, data: option(string)): t =>
  switch (editors) {
  | DebugLoad
  | Examples(_)
  | Exercise(_) => failwith("impossible")
  | Scratch({idx, slides}) =>
    switch (data) {
    | None => editors
    | Some(data) =>
      let state = ScratchSlide.import(data);
      let slides = Util.ListUtil.put_nth(idx, state, slides);
      Scratch({idx, slides});
    }
  };

let switch_example_slide = (editors: t, name: string): option(t) =>
  switch (editors) {
  | DebugLoad
  | Scratch(_)
  | Exercise(_) => None
  | Examples({current, slides})
      when !List.mem_assoc(name, slides) || current == name =>
    None
  | Examples({slides, _}) => Some(Examples({current: name, slides}))
  };
