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
    //let slide = List.nth(slides, n);
    Scratch(n, Util.ListUtil.put_nth(n, ed, slides));
  | Examples(name, slides) =>
    assert(List.mem_assoc(name, slides));
    //let slide = List.assoc(name, slides);
    Examples(
      name,
      slides |> List.remove_assoc(name) |> List.cons((name, ed)),
    );
  | Exercise(n, specs, exercise) =>
    Exercise(n, specs, Exercise.put_editor(exercise, ed))
  };

let get_zipper = (editors: t): Zipper.t => get_editor(editors).state.zipper;

let get_spliced_elabs = (editors: t): list((ModelResults.key, DHExp.t)) => {
  switch (editors) {
  | DebugLoad => []
  | Scratch(n, slides) =>
    let slide = List.nth(slides, n);
    ScratchSlide.spliced_elabs(slide);
  | Examples(name, slides) =>
    let slide = List.assoc(name, slides);
    ScratchSlide.spliced_elabs(slide);
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
