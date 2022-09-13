open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Scratch(int, list(ScratchSlide.state))
  | School(int, list(SchoolExercise.state));

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch = (int, list(ScratchSlide.state));

[@deriving (show({with_path: false}), sexp, yojson)]
type school = (int, list(SchoolExercise.state));

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Scratch
  | School;

let rotate_mode = (editors: t) =>
  switch (editors) {
  | Scratch(_) => School
  | School(_) => Scratch
  };

let get_editor_and_id = (editors: t): (Id.t, Editor.t) =>
  switch (editors) {
  | Scratch(n, slides) =>
    assert(n < List.length(slides));
    let slide = List.nth(slides, n);
    let id = ScratchSlide.id_of_state(slide);
    let ed = ScratchSlide.editor_of_state(slide);
    (id, ed);
  | School(n, exercises) =>
    let exercise = List.nth(exercises, n);
    let id = SchoolExercise.id_of_state(exercise);
    let ed = SchoolExercise.editor_of_state(exercise);
    (id, ed);
  };

let get_editor = (editors: t): Editor.t => snd(get_editor_and_id(editors));

let put_editor_and_id = (id: Id.t, ed: Editor.t, eds: t): t =>
  switch (eds) {
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
  | School(n, exercises) =>
    let exercise = List.nth(exercises, n);
    School(
      n,
      Util.ListUtil.put_nth(
        n,
        SchoolExercise.put_editor_and_id(exercise, id, ed),
        exercises,
      ),
    );
  };

let get_zipper = (editors: t): Zipper.t => get_editor(editors).state.zipper;

let get_spliced_elabs = (editors: t): list((ModelResults.key, DHExp.t)) => {
  switch (editors) {
  | Scratch(n, slides) =>
    let slide = List.nth(slides, n);
    ScratchSlide.spliced_elabs(slide);
  | School(n, exercises) =>
    let exercise = List.nth(exercises, n);
    SchoolExercise.spliced_elabs(exercise);
  };
};

let set_instructor_mode = (editors: t, instructor_mode: bool): t =>
  switch (editors) {
  | Scratch(_) => editors
  | School(n, exercises) =>
    let exercise = List.nth(exercises, n);
    School(
      n,
      Util.ListUtil.put_nth(
        n,
        SchoolExercise.set_instructor_mode(exercise, instructor_mode),
        exercises,
      ),
    );
  };

let num_slides = (editors: t): int =>
  switch (editors) {
  | Scratch(_, slides) => List.length(slides)
  | School(_, exercises) => List.length(exercises)
  };

let cur_slide = (editors: t): int =>
  switch (editors) {
  | Scratch(n, _)
  | School(n, _) => n
  };
