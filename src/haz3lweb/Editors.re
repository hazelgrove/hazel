open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type exercise = {
  idx: int,
  slides: Exercise.s,
  instructor_mode: bool,
};
//TODO(andrew): make sure something analogous to below gets integrated:
/*
 let deserialize = s => {
   let settings = s |> Sexplib.Sexp.of_string |> t_of_sexp;
   if (settings.instructor_mode && !ExerciseSettings.show_instructor) {
     {...settings, instructor_mode: false};
   } else {
     settings;
   };
 };
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  scratch: ScratchSlidesInit.t,
  examples: Examples.t,
  exercise,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | DebugLoad
  | Scratch
  | Examples
  | Exercise;

let exercise_default = {
  //TODO(andrew): instructor_mode, cleanup
  let slides =
    List.map(
      spec =>
        Exercise.{
          spec,
          state: Exercise.state_of_spec(spec, ~instructor_mode=false),
        },
      ExerciseSettings.exercises,
    );
  {idx: 0, slides, instructor_mode: ExerciseSettings.show_instructor};
};

let default: t = {
  scratch: ScratchSlidesInit.default,
  examples: Examples.default,
  exercise: exercise_default,
};

let key: string = "EDITORS";
let serialize = s => s |> yojson_of_t |> Yojson.Safe.to_string;
let deserialize = s => s |> Yojson.Safe.from_string |> t_of_yojson;

let mode_of_string = (s: string): mode =>
  switch (s) {
  | "Scratch" => Scratch
  | "Examples" => Examples
  | "Exercise" => Exercise
  | _ => Scratch
  };

let get_editor_and_id = (mode: mode, editors: t): (Id.t, Editor.t) =>
  switch (mode) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch =>
    let idx = editors.scratch.idx;
    let slides = editors.scratch.slides;
    assert(idx < List.length(slides));
    let slide = List.nth(slides, idx);
    let id = ScratchSlide.id_of_state(slide);
    let ed = ScratchSlide.editor_of_state(slide);
    (id, ed);
  | Examples =>
    let current = editors.examples.current;
    let slides = editors.examples.slides;
    assert(List.mem_assoc(current, slides));
    let slide = List.assoc(current, slides);
    let id = ScratchSlide.id_of_state(slide);
    let ed = ScratchSlide.editor_of_state(slide);
    (id, ed);
  | Exercise =>
    let idx = editors.exercise.idx;
    let slides = editors.exercise.slides;
    assert(idx < List.length(slides));
    let slide = List.nth(slides, idx);
    let id = Exercise.id_of_state(slide.state);
    let ed = Exercise.editor_of_state(slide.state);
    (id, ed);
  };

let get_editor = (mode: mode, editors: t): Editor.t =>
  snd(get_editor_and_id(mode, editors));

let put_editor_and_id = (id: Id.t, ed: Editor.t, mode: mode, editors: t): t =>
  switch (mode) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch =>
    let idx = editors.scratch.idx;
    let slides = editors.scratch.slides;
    assert(idx < List.length(slides));
    let slide = List.nth(slides, idx);
    let slides =
      Util.ListUtil.put_nth(
        idx,
        ScratchSlide.put_editor_and_id(slide, id, ed),
        slides,
      );
    {
      ...editors,
      scratch: {
        idx,
        slides,
      },
    };
  | Examples =>
    let Examples.{current, slides} = editors.examples;
    assert(List.mem_assoc(current, slides));
    let slide = List.assoc(current, slides);
    let slides =
      slides
      |> List.remove_assoc(current)
      |> List.cons((current, ScratchSlide.put_editor_and_id(slide, id, ed)));
    {
      ...editors,
      examples: {
        current,
        slides,
      },
    };
  | Exercise =>
    let {idx, slides, instructor_mode} = editors.exercise;
    let slide = List.nth(slides, idx);
    let slides =
      Util.ListUtil.put_nth(
        idx,
        Exercise.{
          spec: slide.spec,
          state: Exercise.put_editor_and_id(slide.state, id, ed),
        },
        slides,
      );
    {
      ...editors,
      exercise: {
        idx,
        slides,
        instructor_mode,
      },
    };
  };

let get_zipper = (mode: mode, editors: t): Zipper.t =>
  get_editor(mode, editors).state.zipper;

let get_spliced_elabs =
    (mode: mode, editors: t): list((ModelResults.key, DHExp.t)) => {
  switch (mode) {
  | DebugLoad => []
  | Scratch =>
    let idx = editors.scratch.idx;
    let slides = editors.scratch.slides;
    let slide = List.nth(slides, idx);
    ScratchSlide.spliced_elabs(slide);
  | Examples =>
    let Examples.{current, slides} = editors.examples;
    let slide = List.assoc(current, slides);
    ScratchSlide.spliced_elabs(slide);
  | Exercise =>
    let {idx, slides, _} = editors.exercise;
    let slide = List.nth(slides, idx);
    Exercise.spliced_elabs(slide.state);
  };
};

let toggle_instructor_mode = (editors: t): t => {
  let instructor_mode = !editors.exercise.instructor_mode;
  let idx = editors.exercise.idx;
  let slides = editors.exercise.slides;
  let slide = List.nth(slides, idx);
  /* Reset selected editor to prelude, set prelude to read-only */
  let new_slide =
    Exercise.{
      spec: slide.spec,
      state: Exercise.set_instructor_mode(slide.state, instructor_mode),
    };
  let slides = Util.ListUtil.put_nth(idx, new_slide, slides);
  let exercise = {idx, slides, instructor_mode};
  {...editors, exercise};
};

let reset_current = (mode: mode, editors: t): t =>
  switch (mode) {
  | DebugLoad => failwith("impossible")
  | Scratch =>
    let idx = editors.scratch.idx;
    let slides = editors.scratch.slides;
    let slides =
      Util.ListUtil.put_nth(idx, ScratchSlidesInit.init_nth(idx), slides);
    {
      ...editors,
      scratch: {
        idx,
        slides,
      },
    };
  | Examples =>
    let Examples.{current, slides} = editors.examples;
    let slides =
      slides
      |> List.remove_assoc(current)
      |> List.cons((current, Examples.init_name(current)));
    {
      ...editors,
      examples: {
        current,
        slides,
      },
    };
  | Exercise =>
    let {idx, slides, instructor_mode} = editors.exercise;
    let slide = List.nth(slides, idx);
    let slides =
      Util.ListUtil.put_nth(
        idx,
        Exercise.{
          spec: slide.spec,
          state: Exercise.state_of_spec(slide.spec, ~instructor_mode),
        },
        slides,
      );
    {
      ...editors,
      exercise: {
        idx,
        slides,
        instructor_mode,
      },
    };
  };

//TODO(andrew): rename to import_current_scratch or generalize
let import_current = (editors: t, data: option(string)): t => {
  let idx = editors.scratch.idx;
  let slides = editors.scratch.slides;
  switch (data) {
  | None => editors
  | Some(data) =>
    let state = ScratchSlide.import(data);
    let slides = Util.ListUtil.put_nth(idx, state, slides);
    {
      ...editors,
      scratch: {
        idx,
        slides,
      },
    };
  };
};

let switch_example_slide = (editors: t, name: string): option(t) => {
  let Examples.{current, slides} = editors.examples;
  if (!List.mem_assoc(name, slides) || current == name) {
    None;
  } else {
    Some({
      ...editors,
      examples: {
        current: name,
        slides,
      },
    });
  };
};

let switch_scratch_slide = (mode: mode, editors: t, new_idx: int): option(t) =>
  switch (mode) {
  | DebugLoad
  | Examples => None
  | Scratch =>
    let ScratchSlidesInit.{idx, slides} = editors.scratch;
    if (idx == new_idx) {
      None;
    } else {
      Some({
        ...editors,
        scratch: {
          idx: new_idx,
          slides,
        },
      });
    };
  | Exercise =>
    let {idx, slides, instructor_mode} = editors.exercise;
    if (idx == new_idx) {
      None;
    } else {
      Some({
        ...editors,
        exercise: {
          idx: new_idx,
          slides,
          instructor_mode,
        },
      });
    };
  };

let switch_exercise_editor = (editors: t, ~pos): option(t) => {
  let {idx, slides, instructor_mode} = editors.exercise;
  assert(idx < List.length(slides));
  let slide = List.nth(slides, idx);
  let state =
    Exercise.switch_editor(~pos, instructor_mode, ~exercise=slide.state);
  let slides =
    Util.ListUtil.put_nth(idx, Exercise.{spec: slide.spec, state}, slides);
  Some({
    ...editors,
    exercise: {
      idx,
      slides,
      instructor_mode,
    },
  });
};
