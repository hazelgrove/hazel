open Virtual_dom.Vdom;
open Node;
open Widgets;

let option_view = (name, n) =>
  option(
    ~attr=n == name ? Attr.create("selected", "selected") : Attr.many([]),
    [text(n)],
  );

let mode_menu = (~inject, ~mode: Editors.mode) =>
  div(
    ~attr=Attr.many([Attr.class_("mode-name"), Attr.title("Toggle Mode")]),
    [
      select(
        ~attr=
          Attr.on_change((_, name) =>
            inject(Update.SetMode(Editors.mode_of_string(name)))
          ),
        List.map(
          option_view(Editors.show_mode(mode)),
          ["Scratch", "Examples", "Exercise"],
        ),
      ),
    ],
  );

let slide_select = (~inject, ~cur_slide, ~num_slides) => {
  let next_ed = (cur_slide + 1) mod num_slides;
  let prev_ed = Util.IntUtil.modulo(cur_slide - 1, num_slides);
  [
    button(Icons.back, _ => inject(Update.SwitchScratchSlide(prev_ed))),
    text(Printf.sprintf("%d / %d", cur_slide + 1, num_slides)),
    button(Icons.forward, _ => inject(Update.SwitchScratchSlide(next_ed))),
  ];
};

let scratch_view = (~inject, ~cur_slide, ~slides) =>
  [mode_menu(~inject, ~mode=Scratch)]
  @ slide_select(~inject, ~cur_slide, ~num_slides=List.length(slides));

let examples_view = (~inject, ~name, ~editors) => [
  mode_menu(~inject, ~mode=Examples),
  select(
    ~attr=
      Attr.on_change((_, name) => inject(Update.SwitchExampleSlide(name))),
    List.map(option_view(name), List.map(fst, editors)),
  ),
];

let instructor_toggle = (~inject, ~instructor_mode) =>
  ExerciseSettings.show_instructor
    ? [
      toggle("🎓", ~tooltip="Toggle Instructor Mode", instructor_mode, _ =>
        inject(Update.Set(InstructorMode))
      ),
    ]
    : [];

let exercises_view = (~inject, ~cur_slide, ~specs, ~instructor_mode) => {
  [mode_menu(~inject, ~mode=Exercise)]
  @ instructor_toggle(~inject, ~instructor_mode)
  @ slide_select(~inject, ~cur_slide, ~num_slides=List.length(specs));
};

let view =
    (
      ~inject: Update.t => 'a,
      ~editors: Editors.t,
      ~settings as {instructor_mode, _}: ModelSettings.t,
    )
    : Node.t => {
  let contents =
    switch (editors) {
    | DebugLoad => []
    | Scratch(cur_slide, slides) =>
      scratch_view(~inject, ~cur_slide, ~slides)
    | Examples(name, editors) => examples_view(~inject, ~name, ~editors)
    | Exercise(cur_slide, specs, _) =>
      exercises_view(~cur_slide, ~specs, ~inject, ~instructor_mode)
    };
  div(~attr=Attr.id("editor-mode"), contents);
};
