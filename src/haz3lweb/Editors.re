open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | DebugLoad
  | Scratch(int, list(ScratchSlide.state))
  | School(int, list(SchoolExercise.spec), SchoolExercise.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch = (int, list(ScratchSlide.state));

[@deriving (show({with_path: false}), sexp, yojson)]
type school = (int, list(SchoolExercise.spec), SchoolExercise.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | DebugLoad
  | Scratch
  | School;

let rotate_mode = (editors: t) =>
  switch (editors) {
  | DebugLoad => DebugLoad
  | Scratch(_) => School
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
  | School(n, specs, exercise) =>
    School(n, specs, SchoolExercise.put_editor_and_id(exercise, id, ed))
  };

let get_zipper = (editors: t): Zipper.t => get_editor(editors).state.zipper;

let get_ctx_init = (slides, idx) => {
  let stdlib_seg =
    List.nth(slides, Hyper.export_slide) |> snd |> Editor.get_seg;
  let (term, _) = MakeTerm.go(stdlib_seg);
  let info_map = Statics.mk_map(term);
  switch (Id.Map.find_opt(Hyper.export_id, info_map)) {
  | _ when idx == Hyper.export_slide => Ctx.empty
  | None => Ctx.empty
  | Some(info) => Info.ctx_of(info)
  };
};
let get_spliced_elabs =
    (editors: t): list((ModelResults.key, DHExp.t, Environment.t)) => {
  switch (editors) {
  | DebugLoad => []
  | Scratch(idx, slides) =>
    let tests =
      List.nth(slides, Hyper.export_slide)
      |> snd
      |> Editor.get_seg
      |> Interface.eval_segment_to_result
      |> ProgramResult.get_state
      |> EvaluatorState.get_tests
      |> TestMap.lookup(Hyper.export_id);
    let env =
      switch (tests) {
      | Some([(_, _, env), ..._]) => env
      | _ => Environment.empty
      };
    let slide = List.nth(slides, idx);
    ScratchSlide.spliced_elabs(~ctx_init=get_ctx_init(slides, idx), slide)
    |> List.map(((key, dhexp)) => (key, dhexp, env));
  | School(_, _, exercise) => SchoolExercise.spliced_elabs(exercise)
  };
};

let set_instructor_mode = (editors: t, instructor_mode: bool): t =>
  switch (editors) {
  | DebugLoad => failwith("no editors in debug load mode")
  | Scratch(_) => editors
  | School(n, specs, exercise) =>
    School(
      n,
      specs,
      SchoolExercise.set_instructor_mode(exercise, instructor_mode),
    )
  };

let num_slides = (editors: t): int =>
  switch (editors) {
  | DebugLoad => 0
  | Scratch(_, slides) => List.length(slides)
  | School(_, specs, _) => List.length(specs)
  };

let cur_slide = (editors: t): int =>
  switch (editors) {
  | DebugLoad => 0
  | Scratch(n, _)
  | School(n, _, _) => n
  };
