open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch = (int, list(ScratchSlide.state));

[@deriving (show({with_path: false}), sexp, yojson)]
type examples = (string, list((string, ScratchSlide.state)));

[@deriving (show({with_path: false}), sexp, yojson)]
type school = (int, list(SchoolExercise.spec), SchoolExercise.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | DebugLoad
  | Scratch(int, list(ScratchSlide.state))
  | Examples(string, list((string, ScratchSlide.state)))
  | School(int, list(SchoolExercise.spec), SchoolExercise.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | DebugLoad
  | Scratch
  | Examples
  | School;

let rotate_mode = (editors: t) =>
  switch (editors) {
  | DebugLoad => DebugLoad
  | Scratch(_) => Examples
  | Examples(_) => School
  | School(_) => Scratch
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
  | School(_, _, exercise) =>
    let id = SchoolExercise.id_of_state(exercise);
    let ed = SchoolExercise.editor_of_state(exercise);
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
  | School(n, specs, exercise) =>
    School(n, specs, SchoolExercise.put_editor_and_id(exercise, id, ed))
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
  | School(_, _, exercise) => SchoolExercise.spliced_elabs(exercise)
  };
};

let set_instructor_mode = (editors: t, instructor_mode: bool): t =>
  switch (editors) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(_)
  | Examples(_) => editors
  | School(n, specs, exercise) =>
    School(
      n,
      specs,
      SchoolExercise.set_instructor_mode(exercise, instructor_mode),
    )
  };
