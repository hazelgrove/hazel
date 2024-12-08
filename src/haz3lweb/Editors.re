open Util;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch = (int, list(ScratchSlide.state));

[@deriving (show({with_path: false}), sexp, yojson)]
type examples = (string, list((string, ScratchSlide.state)));

[@deriving (show({with_path: false}), sexp, yojson)]
type exercises = (int, list(Exercise.spec), Exercise.state);

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Scratch(int, list(ScratchSlide.state))
  | Documentation(string, list((string, ScratchSlide.state)))
  | Tutorial(string, list((string, Tutorial.state)))
  | Exercises(int, list(Exercise.spec), Exercise.state);

let get_editor = (editors: t): Editor.t =>
  switch (editors) {
  | Scratch(n, slides) =>
    assert(n < List.length(slides));
    List.nth(slides, n).hidden_tests.tests;
  | Documentation(name, slides) =>
    assert(List.mem_assoc(name, slides));
    List.assoc(name, slides).hidden_tests.tests;
  | Tutorial(name, slides) =>
    assert(List.mem_assoc(name, slides));
    let slide_state = List.assoc(name, slides);
    Tutorial.editor_of_state(slide_state);
  | Exercises(_, _, exercise) => Exercise.editor_of_state(exercise)
  };

let update_assoc = ((k, v), lst) =>
  List.map(((k', v')) => k == k' ? (k, v) : (k', v'), lst);

let put_editor = (ed: Editor.t, eds: t): t =>
  switch (eds) {
  | Scratch(n, slides) =>
    assert(n < List.length(slides));
    let originalSlide = List.nth(slides, n);
    let updatedSlide: ScratchSlide.state = {
      title: originalSlide.title,
      description: originalSlide.description,
      hidden_tests: {
        tests: ed,
        hints: originalSlide.hidden_tests.hints,
      },
    };
    Scratch(n, Util.ListUtil.put_nth(n, updatedSlide, slides));
  | Documentation(name, slides) =>
    assert(List.mem_assoc(name, slides));
    // let originalSlide = List.assoc(name, slides);
    // let updatedSlide: ScratchSlide.state = {
    //   title: originalSlide.title,
    //   description: originalSlide.description,
    //   hidden_tests: {
    //     tests: ed,
    //     hints: originalSlide.hidden_tests.hints,
    //   },
    // };
    // Documentation(
    //   name,
    //   slides |> ListUtil.update_assoc((name, updatedSlide)),
    // );
    let new_ed: ScratchSlide.state = {
      title: "",
      description: "",
      hidden_tests: {
        tests: ed,
        hints: [],
      },
    };
    Documentation(name, slides |> update_assoc((name, new_ed)));

  | Tutorial(name, slides) =>
    assert(List.mem_assoc(name, slides));

    // Function to update the slide based on `editor`
    let update_slide =
        (hint: string, state: Tutorial.state): (string, Tutorial.state) =>
      if (hint == name) {
        print_endline(hint);
        print_endline(name);

        let updatedState = Tutorial.put_editor(state, ed);

        (hint, updatedState);
      } else {
        (hint, state);
      };

    let updatedSlides =
      List.map(
        slide => {
          let (hint, state) = slide;
          update_slide(hint, state);
        },
        slides,
      );

    Tutorial(name, updatedSlides);
  | Exercises(n, specs, exercise) =>
    Exercises(n, specs, Exercise.put_editor(exercise, ed))
  };

let update = (f: Editor.t => Editor.t, editors: t): t =>
  editors |> get_editor |> f |> put_editor(_, editors);

let update_opt = (editors: t, f: Editor.t => option(Editor.t)): option(t) =>
  editors |> get_editor |> f |> Option.map(put_editor(_, editors));

let perform_action =
    (~settings: CoreSettings.t, editors: t, a: Action.t)
    : UpdateAction.Result.t(t) => {
  let settings =
    switch (editors) {
    | Exercises(_) =>
      /* If we're in exercises mode, statics is calculated externally,
       * so we set it to off here to disable internal calculation*/
      CoreSettings.on
    | _ => settings
    };
  switch (Perform.go(~settings, a, get_editor(editors))) {
  | Error(err) => Error(FailedToPerform(err))
  | Ok(ed) => Ok(put_editor(ed, editors))
  };
};

let update_current_editor_statics = settings =>
  update(Editor.update_statics(~settings));

let get_ctx_init = (~settings as _: Settings.t, editors: t): Ctx.t =>
  switch (editors) {
  | Scratch(_)
  | Exercises(_)
  | Documentation(_) => Builtins.ctx_init
  | Tutorial(_) => Builtins.ctx_init
  };

let get_env_init = (~settings as _: Settings.t, editors: t): Environment.t =>
  switch (editors) {
  | Scratch(_)
  | Exercises(_)
  | Documentation(_) => Builtins.env_init
  | Tutorial(_) => Builtins.env_init
  };

/* Each mode (e.g. Scratch, School) requires
   elaborating on some number of expressions
   that are spliced together from the editors
   in the mode. Each elaborated expression
   is given a key for later lookup by the mode.

   Used in the Update module */
let get_spliced_elabs =
    (~settings: CoreSettings.t, editors: t)
    : list((ModelResults.key, Elaborator.Elaboration.t)) =>
  switch (editors) {
  | Scratch(idx, _) =>
    let key = ScratchSlide.scratch_key(idx |> string_of_int);
    let statics = get_editor(editors).state.meta.statics;
    let d = Interface.elaborate(~settings, statics.info_map, statics.term);
    [(key, {d: d})];
  | Documentation(name, _) =>
    let key = ScratchSlide.scratch_key(name);
    let statics = get_editor(editors).state.meta.statics;
    let d = Interface.elaborate(~settings, statics.info_map, statics.term);
    [(key, {d: d})];
  | Tutorial(name, slides) =>
    let slideState = List.assoc(name, slides);
    Tutorial.spliced_elabs(settings, slideState);
  | Exercises(_, _, exercise) => Exercise.spliced_elabs(settings, exercise)
  };

let set_instructor_mode = (editors: t, instructor_mode: bool): t =>
  switch (editors) {
  | Scratch(_)
  | Documentation(_) => editors
  | Tutorial(name, slides) =>
    // Assuming you want to pass instructor_mode down to each slide
    let updated_slides =
      List.map(
        ((slide_name, slide_state)) => {
          (
            slide_name,
            Tutorial.set_instructor_mode(slide_state, instructor_mode),
          )
        },
        slides,
      );
    Tutorial(name, updated_slides);
  | Exercises(n, specs, exercise) =>
    Exercises(
      n,
      specs,
      Exercise.set_instructor_mode(exercise, instructor_mode),
    )
  };

let reset_nth_slide =
    (~settings: CoreSettings.t, n, slides): list(ScratchSlide.state) => {
  let (_, init_editors, _) = Init.startup.scratch;
  let data = List.nth(init_editors, n);
  let init_nth = ScratchSlide.unpersist(~settings, data);

  // Wrap the unpersisted Editor.t in a ScratchSlide.state structure
  let updated_slide: ScratchSlide.state = {
    title: init_nth.title, // Assuming title and other fields are available
    description: init_nth.description,
    hidden_tests: {
      tests: init_nth.hidden_tests.tests,
      hints: init_nth.hidden_tests.hints,
    },
  };
  Util.ListUtil.put_nth(n, updated_slide, slides);
};

let reset_named_slide =
    (~settings: CoreSettings.t, name, slides)
    : list((string, ScratchSlide.state)) => {
  let (_, init_editors, _) = Init.startup.documentation;
  let data = List.assoc(name, init_editors);
  let init_name = ScratchSlide.unpersist(~settings, data);

  // Wrap the unpersisted Editor.t in a ScratchSlide.state structure
  let updated_slide: ScratchSlide.state = {
    title: init_name.title,
    description: init_name.description,
    hidden_tests: {
      tests: init_name.hidden_tests.tests,
      hints: init_name.hidden_tests.hints,
    },
  };
  slides |> List.remove_assoc(name) |> List.cons((name, updated_slide));
};

let reset_current =
    (editors: t, ~settings: CoreSettings.t, ~instructor_mode: bool): t =>
  switch (editors) {
  | Scratch(n, slides) => Scratch(n, reset_nth_slide(~settings, n, slides))
  | Documentation(name, slides) =>
    Documentation(name, reset_named_slide(~settings, name, slides))
  | Tutorial(name, slides) =>
    let from_tup = ((word: string, status: Tutorial.state)) => {
      let your_impl_zipper = status.eds.your_impl.state.zipper;
      let hidden_tests_zipper = status.eds.hidden_tests.tests.state.zipper;
      let spec: Tutorial.spec = {
        title: status.eds.title,
        description: status.eds.description,
        your_impl: your_impl_zipper,
        hidden_tests: {
          tests: hidden_tests_zipper,
          hints: status.eds.hidden_tests.hints,
        },
      };

      let new_state =
        Tutorial.state_of_spec(spec, ~settings, ~instructor_mode);
      (word, new_state);
    };

    let updatedSlides = List.map(from_tup, slides);

    Tutorial(name, updatedSlides);

  | Exercises(n, specs, _) =>
    Exercises(
      n,
      specs,
      List.nth(specs, n)
      |> Exercise.state_of_spec(~settings, ~instructor_mode),
    )
  };

let import_current = (~settings, editors: t, data: option(string)): t =>
  switch (editors) {
  | Documentation(_)
  | Tutorial(_)
  | Exercises(_) => failwith("impossible")
  | Scratch(idx, slides) =>
    switch (data) {
    | None => editors
    | Some(data) =>
      let state = ScratchSlide.import(~settings, data);
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
  | Tutorial(_, slides) => Some(Tutorial(name, slides))
  };
